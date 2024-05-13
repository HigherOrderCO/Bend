use super::{AssignPattern, Definition, Enum, Expr, InPlaceOp, MapKey, MatchArm, Stmt, Variant};
use crate::{
  fun::{parser::ParserCommons, Adt, Book, CtrField, Name, Op, STRINGS},
  maybe_grow,
};
use TSPL::Parser;

const PREC: &[&[Op]] =
  &[&[Op::EQL, Op::NEQ], &[Op::LTN], &[Op::GTN], &[Op::ADD, Op::SUB], &[Op::MUL, Op::DIV]];

#[derive(Debug)]
struct Indent(isize);

impl Indent {
  fn enter_level(&mut self) {
    self.0 += 2;
  }

  fn exit_level(&mut self) {
    self.0 -= 2;
  }
}

pub struct PyParser<'i> {
  pub input: &'i str,
  pub index: usize,
}

impl<'a> PyParser<'a> {
  pub fn new(input: &'a str) -> Self {
    Self { input, index: 0 }
  }
}

impl<'a> ParserCommons<'a> for PyParser<'a> {}

impl<'a> Parser<'a> for PyParser<'a> {
  fn input(&mut self) -> &'a str {
    self.input
  }

  fn index(&mut self) -> &mut usize {
    &mut self.index
  }

  /// Generates an error message for parsing failures, including the highlighted context.
  ///
  /// Override to have our own error message.
  fn expected<T>(&mut self, exp: &str) -> Result<T, String> {
    let ini_idx = *self.index();
    let end_idx = *self.index() + 1;
    self.expected_spanned(exp, ini_idx, end_idx)
  }

  /// Consumes an instance of the given string, erroring if it is not found.
  ///
  /// Override to have our own error message.
  fn consume(&mut self, text: &str) -> Result<(), String> {
    self.skip_trivia();
    if self.input().get(*self.index() ..).unwrap_or_default().starts_with(text) {
      *self.index() += text.len();
      Ok(())
    } else {
      self.expected(format!("'{text}'").as_str())
    }
  }
}

impl<'a> PyParser<'a> {
  fn parse_expression(&mut self) -> Result<Expr, String> {
    let Some(head) = self.skip_peek_one() else { return self.expected("primary term")? };
    let res = match head {
      '(' => {
        self.consume("(")?;
        let head = self.parse_infix_or_lambda()?;
        if self.skip_starts_with(",") {
          let mut els = vec![head];
          while self.try_consume(",") {
            els.push(self.parse_infix_or_lambda()?);
          }
          self.consume(")")?;
          Expr::Tup { els }
        } else {
          self.consume(")")?;
          head
        }
      }
      '{' => {
        self.consume("{")?;
        let head = self.parse_infix_or_lambda()?;
        if self.skip_starts_with(":") { self.parse_map_init(head)? } else { self.parse_sup(head)? }
        // let mut entries = Vec::new();
        // loop {
        //   entries.push(self.parse_map_entry()?);
        //   if self.skip_starts_with("}") {
        //     break;
        //   }
        //   self.consume(",")?;
        // }
        // self.consume("}")?;
        // Expr::MapInit { entries }
      }
      '[' => self.list_or_comprehension()?,
      '`' => Expr::Num { val: self.parse_symbol()? },
      '\"' => {
        let str = self.parse_quoted_string()?;
        let val = STRINGS.get(str);
        Expr::Str { val }
      }
      c if c.is_ascii_digit() => {
        let val = self.parse_u32()?;
        Expr::Num { val }
      }
      _ => {
        let nam = self.parse_bend_name()?;
        if self.skip_starts_with("[") {
          self.consume("[")?;
          let key = self.parse_infix_or_lambda()?;
          self.consume("]")?;
          return Ok(Expr::MapGet { nam, key: Box::new(key) });
        } else if self.skip_starts_with("{") {
          let kwargs = self.list_like(|p| p.data_kwarg(), "{", "}", ",", false, 0)?;
          return Ok(Expr::Constructor { name: nam, args: Vec::new(), kwargs });
        }
        Expr::Var { nam }
      }
    };
    Ok(res)
  }

  fn parse_map_init(&mut self, head: Expr) -> Result<Expr, String> {
    let mut entries = Vec::new();
    let map_key = match head {
      Expr::Num { val } => MapKey(val),
      _ => self.expected("Number keyword")?,
    };
    self.consume(":")?;
    let val = self.parse_expression()?;
    entries.push((map_key, val));
    self.try_consume(",");
    let tail = self.list_like(|p| p.parse_map_entry(), "", "}", ",", false, 0)?;
    entries.extend(tail);
    Ok(Expr::MapInit { entries })
  }

  fn parse_sup(&mut self, head: Expr) -> Result<Expr, String> {
    let mut els = vec![head];
    loop {
      if self.skip_starts_with("}") {
        break;
      }
      els.push(self.parse_infix_or_lambda()?);
    }
    self.consume("}")?;
    Ok(Expr::Sup { els })
  }

  fn data_kwarg(&mut self) -> Result<(Name, Expr), String> {
    let nam = self.parse_bend_name()?;
    self.consume(":")?;
    let expr = self.parse_infix_or_lambda()?;
    Ok((nam, expr))
  }

  fn parse_map_entry(&mut self) -> Result<(MapKey, Expr), String> {
    let key = self.parse_map_key()?;
    self.consume(":")?;
    let val = self.parse_expression()?;
    Ok((key, val))
  }

  fn list_or_comprehension(&mut self) -> Result<Expr, String> {
    self.consume("[")?;
    let head = self.parse_infix_or_lambda()?;
    if self.try_consume_keyword("for") {
      let bind = self.parse_bend_name()?;
      self.consume("in")?;
      let iter = self.parse_infix_or_lambda()?;
      let mut cond = None;
      if self.try_consume_keyword("if") {
        cond = Some(Box::new(self.parse_infix_or_lambda()?));
      }
      self.consume("]")?;
      Ok(Expr::Comprehension { term: Box::new(head), bind, iter: Box::new(iter), cond })
    } else {
      let mut head = vec![head];
      self.try_consume(",");
      let tail = self.list_like(|p| p.parse_infix_or_lambda(), "", "]", ",", false, 0)?;
      head.extend(tail);
      Ok(Expr::Lst { els: head })
    }
  }

  fn parse_infix_or_lambda(&mut self) -> Result<Expr, String> {
    if self.try_consume_keyword("lam") | self.try_consume("λ") {
      let names = self.list_like(|p| p.parse_bend_name(), "", ":", ",", false, 1)?;
      let bod = self.parse_infix_or_lambda()?;
      Ok(Expr::Lam { names, bod: Box::new(bod) })
    } else {
      self.parse_infix(0)
    }
  }

  fn parse_call(&mut self) -> Result<Expr, String> {
    let mut args = Vec::new();
    let mut kwargs = Vec::new();
    let fun = self.parse_expression()?;
    self.advance_inline_trivia();
    if self.try_consume("(") {
      let mut must_be_named = false;
      while !self.try_consume(")") {
        let ini_idx = *self.index();
        let (bnd, arg) = self.parse_named_arg()?;
        let end_idx = *self.index();
        if let Some(bnd) = bnd {
          must_be_named = true;
          kwargs.push((bnd, arg));
        } else if must_be_named {
          let msg = "Positional argument are not allowed to go after named arguments.".to_string();
          return self.with_ctx(Err(msg), ini_idx, end_idx);
        } else {
          args.push(arg);
        }
        self.try_consume(",");
      }
    }
    if args.is_empty() && kwargs.is_empty() {
      Ok(fun)
    } else {
      Ok(Expr::Call { fun: Box::new(fun), args, kwargs })
    }
  }

  fn parse_named_arg(&mut self) -> Result<(Option<Name>, Expr), String> {
    let arg = self.parse_infix_or_lambda()?;
    if self.try_consume("=") {
      if let Expr::Var { nam } = arg {
        let bind = Some(nam);
        let arg = self.parse_infix_or_lambda()?;
        Ok((bind, arg))
      } else {
        let msg = "Unexpected '=' in unnamed argument.".to_string();
        let idx = *self.index();
        self.with_ctx(Err(msg), idx, idx + 1)
      }
    } else {
      Ok((None, arg))
    }
  }

  fn parse_infix(&mut self, prec: usize) -> Result<Expr, String> {
    maybe_grow(|| {
      self.advance_inline_trivia();
      if prec > PREC.len() - 1 {
        return self.parse_call();
      }
      let mut lhs = self.parse_infix(prec + 1)?;
      while let Some(op) = self.get_op() {
        if PREC[prec].iter().any(|r| *r == op) {
          let op = self.parse_oper()?;
          let rhs = self.parse_infix(prec + 1)?;
          self.advance_inline_trivia();
          lhs = Expr::Bin { op, lhs: Box::new(lhs), rhs: Box::new(rhs) };
        } else {
          break;
        }
      }
      Ok(lhs)
    })
  }

  fn get_op(&mut self) -> Option<Op> {
    let ret = if self.skip_starts_with("+") {
      Op::ADD
    } else if self.skip_starts_with("-") {
      Op::SUB
    } else if self.skip_starts_with("*") {
      Op::MUL
    } else if self.skip_starts_with("/") {
      Op::DIV
    } else if self.skip_starts_with("%") {
      Op::REM
    } else if self.skip_starts_with("<") {
      Op::LTN
    } else if self.skip_starts_with(">") {
      Op::GTN
    } else if self.skip_starts_with("==") {
      Op::EQL
    } else if self.skip_starts_with("!=") {
      Op::NEQ
    } else {
      return None;
    };
    Some(ret)
  }

  fn skip_exact_indent(&mut self, Indent(mut count): &Indent, block: bool) -> Result<bool, String> {
    let expected = count;
    let ini_idx = *self.index();
    if count <= 0 {
      self.skip_spaces();
      return Ok(false);
    }
    while let Some(c) = self.peek_one() {
      if c == '\n' || c == '\t' || c == '\r' {
        self.advance_one();
      } else {
        break;
      }
    }
    while let Some(c) = self.peek_one() {
      if c.is_ascii_whitespace() {
        count -= 1;
        self.advance_one();
      } else {
        break;
      }
    }
    if block {
      return Ok(count == 0 && !self.is_eof());
    }
    // TODO: add the current line in Err
    if count == 0 {
      Ok(true)
    } else {
      let msg = format!("Indentation error. Expected {} spaces, got {}.", expected, expected - count);
      let end_idx = *self.index();
      self.with_ctx(Err(msg), ini_idx, end_idx)
    }
  }

  fn parse_statement(&mut self, indent: &mut Indent) -> Result<Stmt, String> {
    maybe_grow(|| {
      self.skip_exact_indent(indent, false)?;
      if self.try_consume_keyword("return") {
        self.parse_return()
      } else if self.try_consume_keyword("if") {
        self.parse_if(indent)
      } else if self.try_consume_keyword("match") {
        self.parse_match(indent)
      } else if self.try_consume_keyword("switch") {
        self.parse_switch(indent)
      } else if self.try_consume_keyword("fold") {
        self.parse_fold(indent)
      } else if self.try_consume_keyword("bend") {
        self.parse_bend(indent)
      } else if self.try_consume_keyword("do") {
        self.parse_do(indent)
      } else {
        self.parse_assign(indent)
      }
    })
  }

  fn parse_symbol(&mut self) -> Result<u32, String> {
    self.consume("`")?;
    let mut result = u32::MAX;
    let mut count = 0;
    while count < 4 {
      if self.starts_with("`") {
        break;
      }
      count += 1;
      let Some(c) = self.advance_one() else { self.expected("symbol")? };
      let c = c as u8;
      let nxt = match c {
        b'A' ..= b'Z' => c - b'A',
        b'a' ..= b'z' => c - b'a' + 26,
        b'0' ..= b'9' => c - b'0' + 52,
        b'+' => 62,
        b'/' => 63,
        _ => panic!(),
      };
      result = (result << 6) | nxt as u32;
    }
    self.consume("`")?;
    Ok(result)
  }

  fn parse_map_key(&mut self) -> Result<MapKey, String> {
    if self.skip_starts_with("`") {
      Ok(MapKey(self.parse_symbol()?))
    } else {
      Ok(MapKey(self.parse_u32()? as u32))
    }
  }

  /// Assignments, monadic bind operations and in-place operations.
  fn parse_assign(&mut self, indent: &mut Indent) -> Result<Stmt, String> {
    let ini_idx = *self.index();
    let pat = self.parse_assign_pattern()?;
    let end_idx = *self.index();
    self.advance_inline_trivia();

    // Assignment
    if self.starts_with("=") {
      self.consume("=")?;
      let val = self.parse_infix_or_lambda()?;
      self.consume(";")?;
      let nxt = self.parse_statement(indent)?;
      return Ok(Stmt::Assign { pat, val: Box::new(val), nxt: Box::new(nxt) });
    }
    // Ask
    if self.starts_with("<-") {
      self.consume("<-")?;
      let val = self.parse_infix_or_lambda()?;
      self.consume(";")?;
      let nxt = self.parse_statement(indent)?;
      return Ok(Stmt::Ask { pat, val: Box::new(val), nxt: Box::new(nxt) });
    }
    // In-place
    if let AssignPattern::Var(name) = pat {
      let op = self.parse_in_place_op()?;
      let val = self.parse_infix_or_lambda()?;
      self.consume(";")?;
      let nxt = self.parse_statement(indent)?;
      Ok(Stmt::InPlace { op, var: name, val: Box::new(val), nxt: Box::new(nxt) })
    } else {
      self.expected_spanned("variable name", ini_idx, end_idx)
    }
  }

  fn parse_in_place_op(&mut self) -> Result<InPlaceOp, String> {
    self.advance_inline_trivia();
    if self.starts_with("+=") {
      self.consume("+=")?;
      Ok(InPlaceOp::Add)
    } else if self.starts_with("-=") {
      self.consume("-=")?;
      Ok(InPlaceOp::Sub)
    } else if self.starts_with("*=") {
      self.consume("*=")?;
      Ok(InPlaceOp::Mul)
    } else if self.starts_with("/=") {
      self.consume("/=")?;
      Ok(InPlaceOp::Div)
    } else if self.starts_with("&=") {
      self.consume("&=")?;
      Ok(InPlaceOp::And)
    } else if self.starts_with("|=") {
      self.consume("|=")?;
      Ok(InPlaceOp::Or)
    } else if self.starts_with("^=") {
      self.consume("^=")?;
      Ok(InPlaceOp::Xor)
    } else {
      self.expected("in-place operator")?
    }
  }

  fn parse_return(&mut self) -> Result<Stmt, String> {
    let term = self.parse_infix_or_lambda()?;
    self.consume(";")?;
    Ok(Stmt::Return { term: Box::new(term) })
  }

  fn parse_if(&mut self, indent: &mut Indent) -> Result<Stmt, String> {
    let cond = self.parse_infix_or_lambda()?;
    self.consume(":")?;
    indent.enter_level();
    let then = self.parse_statement(indent)?;
    indent.exit_level();
    self.skip_exact_indent(indent, false)?;
    self.consume("else")?;
    self.consume(":")?;
    indent.enter_level();
    let otherwise = self.parse_statement(indent)?;
    indent.exit_level();
    Ok(Stmt::If { cond: Box::new(cond), then: Box::new(then), otherwise: Box::new(otherwise) })
  }

  fn parse_match(&mut self, indent: &mut Indent) -> Result<Stmt, String> {
    let (bind, arg) = self.parse_match_arg()?;
    self.consume(":")?;
    let mut arms = Vec::new();
    indent.enter_level();
    loop {
      if !self.skip_exact_indent(indent, true)? {
        break;
      }
      arms.push(self.parse_case(indent)?);
    }
    indent.exit_level();
    Ok(Stmt::Match { arg: Box::new(arg), bind, arms })
  }

  fn parse_match_arg(&mut self) -> Result<(Option<Name>, Expr), String> {
    let ini_idx = *self.index();
    let arg = self.parse_infix_or_lambda()?;
    let end_idx = *self.index();

    self.advance_inline_trivia();
    match (arg, self.starts_with("=")) {
      (Expr::Var { nam }, true) => {
        self.consume("=")?;
        Ok((Some(nam), self.parse_infix_or_lambda()?))
      }
      (Expr::Var { nam }, false) => Ok((Some(nam.clone()), Expr::Var { nam })),
      (_, true) => self.expected_spanned("argument name", ini_idx, end_idx),
      (arg, false) => Ok((Some(Name::new("%arg")), arg)),
    }
  }

  fn name_or_wildcard(&mut self) -> Result<Option<Name>, String> {
    self.labelled(
      |p| {
        if p.try_consume("_") {
          Ok(None)
        } else {
          let nam = p.parse_bend_name()?;
          Ok(Some(nam))
        }
      },
      "name or '_'",
    )
  }

  fn parse_case(&mut self, indent: &mut Indent) -> Result<MatchArm, String> {
    self.consume("case")?;
    let pat = self.name_or_wildcard()?;
    self.consume(":")?;
    indent.enter_level();
    let body = self.parse_statement(indent)?;
    indent.exit_level();
    Ok(MatchArm { lft: pat, rgt: body })
  }

  fn parse_switch(&mut self, indent: &mut Indent) -> Result<Stmt, String> {
    let (bind, arg) = self.parse_match_arg()?;
    self.consume(":")?;
    let mut arms = Vec::new();

    indent.enter_level();
    let mut should_continue = true;
    let mut expected_num = 0;

    while should_continue && self.skip_exact_indent(indent, true)? {
      self.consume("case")?;
      if let Some(c) = self.skip_peek_one() {
        match c {
          '_' => {
            if expected_num == 0 {
              return self.expected("0");
            } else {
              self.consume("_")?;
              should_continue = false;
            }
          }
          c if c.is_ascii_digit() => {
            let value = self.parse_u32()?;
            if value != expected_num {
              return self.expected(&expected_num.to_string());
            }
          }
          _ => return self.expected("Number pattern"),
        }
        self.consume(":")?;
        indent.enter_level();
        arms.push(self.parse_statement(indent)?);
        indent.exit_level();
        expected_num += 1;
      } else {
        self.expected("Switch pattern")?
      }
    }
    indent.exit_level();
    Ok(Stmt::Switch { arg: Box::new(arg), bind, arms })
  }

  fn parse_fold(&mut self, indent: &mut Indent) -> Result<Stmt, String> {
    let (bind, arg) = self.parse_match_arg()?;
    self.consume(":")?;
    let mut arms = Vec::new();
    indent.enter_level();
    loop {
      if !self.skip_exact_indent(indent, true)? {
        break;
      }
      arms.push(self.parse_case(indent)?);
    }
    indent.exit_level();
    Ok(Stmt::Fold { arg: Box::new(arg), bind, arms })
  }

  fn parse_bend(&mut self, indent: &mut Indent) -> Result<Stmt, String> {
    let args = self.list_like(|p| p.parse_match_arg(), "", "while", ",", false, 1)?;
    let (bind, init) = args.into_iter().unzip();
    let cond = self.parse_infix_or_lambda()?;
    self.consume(":")?;
    indent.enter_level();
    let step = self.parse_statement(indent)?;
    indent.exit_level();
    self.consume("then")?;
    self.consume(":")?;
    indent.enter_level();
    let base = self.parse_statement(indent)?;
    indent.exit_level();
    Ok(Stmt::Bend { bind, init, cond: Box::new(cond), step: Box::new(step), base: Box::new(base) })
  }

  fn parse_do(&mut self, indent: &mut Indent) -> Result<Stmt, String> {
    let typ = self.parse_bend_name()?;
    self.consume(":")?;

    indent.enter_level();
    let bod = self.parse_statement(indent)?;
    indent.exit_level();

    Ok(Stmt::Do { typ, bod: Box::new(bod) })
  }

  fn parse_assign_pattern(&mut self) -> Result<AssignPattern, String> {
    // Tup pattern
    if self.skip_starts_with("(") {
      let mut binds = self.list_like(|p| p.parse_bend_name(), "(", ")", ",", true, 1)?;
      if binds.len() == 1 {
        return Ok(AssignPattern::Var(std::mem::take(&mut binds[0])));
      } else {
        return Ok(AssignPattern::Tup(binds));
      }
    }
    // Dup pattern
    if self.skip_starts_with("{") {
      let binds = self.list_like(|p| p.parse_bend_name(), "{", "}", "", false, 2)?;
      return Ok(AssignPattern::Sup(binds));
    }

    let var = self.parse_bend_name()?;

    // Map get pattern
    if self.skip_starts_with("[") {
      self.consume("[")?;
      let key = self.parse_infix_or_lambda()?;
      self.consume("]")?;
      return Ok(AssignPattern::MapSet(var, key));
    }

    // Var pattern
    Ok(AssignPattern::Var(var))
  }

  pub fn parse_def(&mut self, indent: usize) -> Result<Definition, String> {
    if indent > 0 {
      self.expected("Indentation error")?;
    }
    let mut indent = Indent(0);

    let name = self.parse_bend_name()?;
    let params = self.list_like(|p| p.parse_bend_name(), "(", ")", ",", false, 0)?;
    self.consume(":")?;
    indent.enter_level();
    let body = self.parse_statement(&mut indent)?;
    indent.exit_level();
    Ok(Definition { name, params, body })
  }

  pub fn parse_data_type(&mut self, indent: usize) -> Result<Enum, String> {
    fn parse_variant_field(p: &mut PyParser) -> Result<CtrField, String> {
      let rec = p.try_consume("~");
      let nam = p.parse_bend_name()?;
      Ok(CtrField { nam, rec })
    }

    if indent > 0 {
      self.expected("Indentation error")?;
    }
    let mut indent = Indent(0);

    let name = self.parse_bend_name()?;
    let mut variants = Vec::new();
    self.consume(":")?;
    indent.enter_level();
    loop {
      if !self.skip_exact_indent(&indent, true)? {
        break;
      }
      let name = Name::new(format!("{name}/{}", self.parse_bend_name()?));
      let mut fields = Vec::new();
      self.take_while(|p| p == ' '); // ?
      if self.starts_with("{") {
        fields = self.list_like(|p| parse_variant_field(p), "{", "}", ",", false, 0)?;
      }
      variants.push((name.clone(), Variant { name, fields }));
    }
    indent.exit_level();
    let variants = variants.into_iter().collect();
    Ok(Enum { name, variants })
  }

  pub fn add_def(
    &mut self,
    mut def: Definition,
    book: &mut Book,
    ini_idx: usize,
    end_idx: usize,
  ) -> Result<(), String> {
    if book.defs.contains_key(&def.name) {
      let msg = format!("Redefinition of function '{}'.", def.name);
      return self.with_ctx(Err(msg), ini_idx, end_idx);
    }
    if book.ctrs.contains_key(&def.name) {
      let msg = format!("Redefinition of constructor '{}'.", def.name);
      return self.with_ctx(Err(msg), ini_idx, end_idx);
    }
    def.order_kwargs(book)?;
    def.gen_map_get();
    let def = def.to_fun();
    book.defs.insert(def.name.clone(), def);
    Ok(())
  }

  pub fn add_enum(
    &mut self,
    r#enum: Enum,
    book: &mut Book,
    ini_idx: usize,
    end_idx: usize,
    builtin: bool,
  ) -> Result<(), String> {
    if book.adts.contains_key(&r#enum.name) {
      let msg = format!("Redefinition of type '{}'.", r#enum.name);
      return self.with_ctx(Err(msg), ini_idx, end_idx);
    }
    let mut adt = Adt { ctrs: Default::default(), builtin };
    for (name, variant) in r#enum.variants {
      if book.defs.contains_key(&name) {
        let msg = format!("Redefinition of function '{}'.", name);
        return self.with_ctx(Err(msg), ini_idx, end_idx);
      }
      if book.ctrs.contains_key(&name) {
        let msg = format!("Redefinition of constructor '{}'.", variant.name);
        return self.with_ctx(Err(msg), ini_idx, end_idx);
      }
      book.ctrs.insert(name, r#enum.name.clone());
      adt.ctrs.insert(variant.name, variant.fields);
    }
    book.adts.insert(r#enum.name.clone(), adt);
    Ok(())
  }
}
