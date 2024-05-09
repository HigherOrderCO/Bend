use std::collections::HashSet;

use indexmap::IndexMap;
use TSPL::Parser;

use crate::{
  fun::{parser::ParserCommons, CtrField, Name, Op, STRINGS},
  maybe_grow,
};

use super::{
  AssignPattern, Definition, Enum, Expr, InPlaceOp, MBind, MapKey, MatchArm, Program, Stmt, TopLevel, Variant,
};

const PREC: &[&[Op]] =
  &[&[Op::EQL, Op::NEQ], &[Op::LTN], &[Op::GTN], &[Op::ADD, Op::SUB], &[Op::MUL, Op::DIV]];

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
  input: &'i str,
  index: usize,
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
    self.skip_trivia();
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
        let mut entries = Vec::new();
        loop {
          entries.push(self.parse_map_entry()?);
          if self.skip_starts_with("}") {
            break;
          }
          self.consume(",")?;
        }
        self.consume("}")?;
        Expr::MapInit { entries }
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
          let key = self.parse_map_key()?;
          self.consume("]")?;
          return Ok(Expr::MapGet { nam, key });
        }
        Expr::Var { nam }
      }
    };
    Ok(res)
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
      let tail = self.list_like(|p| p.parse_infix_or_lambda(), "", "]", ",", true, 0)?;
      head.extend(tail);
      Ok(Expr::Lst { els: head })
    }
  }

  fn parse_infix_or_lambda(&mut self) -> Result<Expr, String> {
    if self.try_consume_keyword("lambda") {
      let names = self.list_like(|p| p.parse_bend_name(), "", ":", ",", true, 1)?;
      let bod = self.parse_infix_or_lambda()?;
      Ok(Expr::Lam { names, bod: Box::new(bod) })
    } else {
      self.parse_infix(0)
    }
  }

  fn parse_call(&mut self) -> Result<Expr, String> {
    self.skip_trivia();
    let mut args = Vec::new();
    let mut kwargs = Vec::new();
    let fun = self.parse_expression()?;
    if self.try_consume("(") {
      loop {
        if self.try_consume(",") {
          continue;
        }
        if self.try_consume(")") {
          break;
        }

        let (bind, arg) = self.parse_named_arg()?;
        if let Some(bind) = bind {
          kwargs.push((bind, arg));
        } else {
          args.push(arg);
        }
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
    match self.skip_peek_one() {
      Some('+') => Some(Op::ADD),
      Some('-') => Some(Op::SUB),
      Some('*') => Some(Op::MUL),
      Some('/') => Some(Op::DIV),
      Some('>') => Some(Op::GTN),
      Some('<') => Some(Op::LTN),
      _ => None,
    }
  }

  fn skip_newlines(&mut self) -> usize {
    loop {
      let num_spaces = self.advance_inline_trivia();
      if self.peek_one() == Some('\r') {
        self.advance_one();
      }
      if self.peek_one() == Some('\n') {
        self.advance_one();
      } else {
        return num_spaces;
      }
    }
  }

  fn advance_inline_trivia(&mut self) -> usize {
    let mut char_count = 0;
    while let Some(c) = self.peek_one() {
      if " \t".contains(c) {
        self.advance_one();
        char_count += 1;
        continue;
      }
      if c == '/' && self.input().get(*self.index() ..).unwrap_or_default().starts_with("//") {
        while let Some(c) = self.peek_one() {
          if c != '\n' {
            self.advance_one();
            char_count += 1;
          } else {
            break;
          }
        }
        continue;
      }
      break;
    }
    char_count
  }

  fn skip_exact_indent(&mut self, Indent(mut count): &Indent, block: bool) -> Result<bool, String> {
    while let Some(c) = self.peek_one() {
      if c == '\n' {
        self.advance_one();
      } else {
        break;
      }
    }
    if count <= 0 {
      self.skip_spaces();
      return Ok(false);
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
    if count == 0 { Ok(true) } else { Err("Indentation error".to_string()) }
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
        self.parse_in_place(indent)
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
      Ok(MapKey(self.parse_u64()? as u32))
    }
  }

  fn parse_in_place(&mut self, indent: &mut Indent) -> Result<Stmt, String> {
    if self.starts_with("(") {
      self.parse_assignment(indent)
    } else {
      let name = self.parse_bend_name()?;
      if self.skip_starts_with("=") {
        // it's actually an assignment
        self.consume("=")?;
        let val = self.parse_infix_or_lambda()?;
        self.consume(";")?;
        let nxt = self.parse_statement(indent)?;
        return Ok(Stmt::Assign { pat: AssignPattern::Var(name), val: Box::new(val), nxt: Box::new(nxt) });
      } else if self.skip_starts_with("[") {
        self.consume("[")?;
        let key = self.parse_map_key()?;
        self.consume("]")?;
        self.consume("=")?;
        let val = self.parse_expression()?;
        self.consume(";")?;
        let nxt = self.parse_statement(indent)?;
        return Ok(Stmt::Assign {
          pat: AssignPattern::MapSet(name, key),
          val: Box::new(val),
          nxt: Box::new(nxt),
        });
      }
      let in_place: InPlaceOp;
      self.skip_spaces();
      if let Some(s) = self.peek_many(2) {
        if s == "+=" {
          self.consume("+=")?;
          in_place = InPlaceOp::Add;
        } else if s == "-=" {
          self.consume("+=")?;
          in_place = InPlaceOp::Sub;
        } else if s == "*=" {
          self.consume("+=")?;
          in_place = InPlaceOp::Mul;
        } else if s == "/=" {
          self.consume("+=")?;
          in_place = InPlaceOp::Div;
        } else {
          return self.expected("in-place operator");
        }

        let val = self.parse_infix_or_lambda()?;
        self.consume(";")?;
        let nxt = self.parse_statement(indent)?;
        Ok(Stmt::InPlace { op: in_place, var: name, val: Box::new(val), nxt: Box::new(nxt) })
      } else {
        self.expected("in-place operator")?
      }
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
            let value = self.parse_u64()?;
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
    let args = self.list_like(|p| p.parse_match_arg(), "", "while", ",", true, 1)?;
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
    let fun = self.parse_bend_name()?;
    self.consume(":")?;

    let mut block = Vec::new();

    indent.enter_level();
    loop {
      if !self.skip_exact_indent(indent, true)? {
        break;
      }
      if self.try_consume("!") {
        let pat = self.parse_assign_pattern()?;
        self.consume("=")?;
        let val = self.parse_infix_or_lambda()?;
        self.consume(";")?;
        block.push(MBind::Ask { pat, val: Box::new(val) });
      } else {
        block.push(MBind::Stmt { stmt: Box::new(self.parse_statement(&mut Indent(0))?) })
      }
    }
    indent.exit_level();

    Ok(Stmt::Do { fun, block })
  }

  fn parse_assignment(&mut self, indent: &mut Indent) -> Result<Stmt, String> {
    let pat = self.parse_assign_pattern()?;
    self.consume("=")?;
    let val = self.parse_infix_or_lambda()?;
    self.consume(";")?;
    let nxt = self.parse_statement(indent)?;
    Ok(Stmt::Assign { pat, val: Box::new(val), nxt: Box::new(nxt) })
  }

  fn parse_assign_pattern(&mut self) -> Result<AssignPattern, String> {
    if self.skip_starts_with("(") {
      let mut binds = self.list_like(|p| p.parse_bend_name(), "(", ")", ",", true, 1)?;
      if binds.len() == 1 {
        Ok(AssignPattern::Var(std::mem::take(&mut binds[0])))
      } else {
        Ok(AssignPattern::Tup(binds))
      }
    } else {
      self.parse_bend_name().map(AssignPattern::Var)
    }
  }

  fn parse_top_level(&mut self, indent: &mut Indent) -> Result<TopLevel, String> {
    self.skip_exact_indent(indent, false)?;
    if self.try_consume_keyword("def") {
      Ok(TopLevel::Def(self.parse_def(indent)?))
    } else if self.try_consume_keyword("enum") {
      Ok(TopLevel::Enum(self.parse_enum(indent)?))
    } else {
      self.expected("Enum or Def declaration")?
    }
  }

  fn parse_def(&mut self, indent: &mut Indent) -> Result<Definition, String> {
    let name = self.parse_bend_name()?;
    let params = self.list_like(|p| p.parse_bend_name(), "(", ")", ",", true, 0)?;
    self.consume(":")?;
    indent.enter_level();
    let body = self.parse_statement(indent)?;
    indent.exit_level();
    Ok(Definition { name, params, body })
  }

  fn parse_enum(&mut self, indent: &mut Indent) -> Result<Enum, String> {
    fn parse_variant_field(p: &mut PyParser) -> Result<CtrField, String> {
      let rec = p.try_consume("~");
      let nam = p.parse_bend_name()?;
      Ok(CtrField { nam, rec })
    }

    let name = self.parse_bend_name()?;
    let mut variants = Vec::new();
    self.consume(":")?;
    indent.enter_level();
    loop {
      if !self.skip_exact_indent(indent, true)? {
        break;
      }
      let name = self.parse_bend_name()?;
      let mut fields = Vec::new();
      if self.skip_starts_with("(") {
        fields = self.list_like(|p| parse_variant_field(p), "(", ")", ",", true, 0)?;
      }
      variants.push((name.clone(), Variant { name, fields }));
    }
    indent.exit_level();
    let variants = variants.into_iter().collect();
    Ok(Enum { name, variants })
  }

  pub fn parse_program(&mut self, builtins: &HashSet<Name>) -> Result<Program, String> {
    let mut enums = IndexMap::<Name, Enum>::new();
    let mut defs = IndexMap::<Name, Definition>::new();
    let mut variants = IndexMap::<Name, Name>::new();

    loop {
      let spaces = self.skip_newlines();
      if self.is_eof() {
        break;
      }
      if spaces != 0 {
        self.expected("Indentation error")?;
      }
      match self.parse_top_level(&mut Indent(0))? {
        TopLevel::Def(def) => Self::add_def(&mut defs, def, builtins)?,
        TopLevel::Enum(r#enum) => Self::add_enum(&mut enums, &mut variants, r#enum, builtins)?,
      }
    }

    Ok(Program { enums, defs, variants })
  }

  fn add_def(
    defs: &mut IndexMap<Name, Definition>,
    def: Definition,
    builtins: &HashSet<Name>,
  ) -> Result<(), String> {
    if builtins.contains(&def.name) {
      return Err(format!("Built-in function '{}' cannot be overridden.", def.name));
    }
    match defs.entry(def.name.clone()) {
      indexmap::map::Entry::Occupied(o) => Err(format!("Repeated function definition '{}'.", o.get().name)),
      indexmap::map::Entry::Vacant(v) => {
        v.insert(def);
        Ok(())
      }
    }
  }

  fn add_enum(
    enums: &mut IndexMap<Name, Enum>,
    variants: &mut IndexMap<Name, Name>,
    r#enum: Enum,
    builtins: &HashSet<Name>,
  ) -> Result<(), String> {
    if builtins.contains(&r#enum.name) {
      return Err(format!("Built-in type '{}' cannot be overridden.", r#enum.name));
    }
    match enums.entry(r#enum.name.clone()) {
      indexmap::map::Entry::Occupied(o) => return Err(format!("Repeated enum '{}'.", o.key())),
      indexmap::map::Entry::Vacant(v) => {
        for (name, variant) in r#enum.variants.iter() {
          if builtins.contains(name) {
            return Err(format!("Built-in constructor '{}' cannot be overridden.", name));
          }
          match variants.entry(name.clone()) {
            indexmap::map::Entry::Occupied(_) => {
              return Err(format!("Repeated variant '{}'.", variant.name));
            }
            indexmap::map::Entry::Vacant(v) => _ = v.insert(r#enum.name.clone()),
          }
        }
        v.insert(r#enum);
      }
    }
    Ok(())
  }
}

#[cfg(test)]
#[allow(dead_code)]
mod test {
  use super::PyParser;

  #[test]
  fn map_get() {
    let src = r#"
def main():
  x = { 2: 1, 3: 2 };
  y = id(x[2]);
  return y + x[3];
  "#;
    let mut parser = PyParser::new(src);
    let mut program = parser.parse_program(&Default::default()).inspect_err(|e| println!("{e}")).unwrap();
    program.gen_map_get();
    let out = program.to_fun(crate::Book::builtins());
    println!("{}", out.display_pretty());
  }

  #[test]
  fn parse_program() {
    let src = r#"
enum Point:
  Point(x, y)

enum Bool:
  True()
  False()


def symbols():
  x = { `x`: 5, 2: 3 };
  x[`x`] = 2;
  x[2] = 3;
  return x[`x`] + `foxy`;

def mk_point():
  return Point(y = 2, x = 1);

def identity(x):
  return x;

def inc(n):
  n += 1;
  return n;

//def inc_list(list):
//  return [x+1 for x in list];

def lam():
  return lambda x, y: x;

def do_match(b):
  match b:
    True:
      return 1;
    False:
      return 0;

def true():
  return True;

def fib(n):
  if n < 2:
    return n;
  else:
    return fib(n - 1) + fib(n - 2);

def swt(n):
  switch n:
    0:
      return 42;
    _:
      return 1;

def fld(list):
  fold list:
    List.cons:
      return 1;
    List.nil:
      return 2;

def bnd():
  bend x = 0 while x < 10:
    return List.cons(x go(x + 1));
  then:
    return List.nil();

//def main():
//  do IO.bind:
//    !x = IO.read();
//    return x;
    "#;
    let mut parser = PyParser::new(src);
    let mut program = parser.parse_program(&Default::default()).inspect_err(|e| println!("{e}")).unwrap();
    program.order_kwargs();
    program.gen_map_get();
    let out = program.to_fun(crate::Book::builtins());
    println!("{out}");
  }
}
