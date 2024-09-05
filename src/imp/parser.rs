use crate::{
  fun::{
    parser::{is_num_char, Indent, ParseResult, ParserCommons},
    CtrField, Name, Num, Op, Source, SourceKind, STRINGS,
  },
  imp::{AssignPattern, Definition, Enum, Expr, InPlaceOp, MatchArm, Stmt, Variant},
  maybe_grow,
};
use TSPL::Parser;

use super::RepeatedNames;

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
  fn expected<T>(&mut self, exp: &str) -> ParseResult<T> {
    let ini_idx = *self.index();
    let end_idx = *self.index() + 1;
    self.expected_spanned(exp, ini_idx..end_idx)
  }

  /// Consumes an instance of the given string, erroring if it is not found.
  ///
  /// Override to have our own error message.
  fn consume(&mut self, text: &str) -> ParseResult<()> {
    self.skip_trivia();
    if self.input().get(*self.index()..).unwrap_or_default().starts_with(text) {
      *self.index() += text.len();
      Ok(())
    } else {
      self.expected(format!("'{text}'").as_str())
    }
  }

  fn skip_trivia(&mut self) {
    while let Some(c) = self.peek_one() {
      if c.is_ascii_whitespace() {
        self.advance_one();
        continue;
      }
      if c == '#' {
        while let Some(c) = self.peek_one() {
          if c != '\n' {
            self.advance_one();
          } else {
            break;
          }
        }
        self.advance_one(); // Skip the newline character as well
        continue;
      }
      break;
    }
  }
}

impl<'a> PyParser<'a> {
  /// <var> <postfix>?
  ///
  fn parse_primary_expr(&mut self, inline: bool) -> ParseResult<Expr> {
    if inline {
      self.skip_trivia_inline()?;
    } else {
      self.skip_trivia();
    }
    if self.try_parse_keyword("lambda") | self.try_consume_exactly("λ") {
      fn parse_lam_var(p: &mut PyParser) -> ParseResult<(Name, bool)> {
        if p.starts_with("$") {
          p.advance_one();
          Ok((p.parse_bend_name()?, true))
        } else {
          Ok((p.parse_bend_name()?, false))
        }
      }
      let names = self.list_like(|p| parse_lam_var(p), "", ":", ",", false, 1)?;
      let bod = self.parse_expr(inline, false)?;
      Ok(Expr::Lam { names, bod: Box::new(bod) })
    } else if self.starts_with("(") {
      self.advance_one();
      let expr = self.parse_expr(inline, true)?;
      self.consume(")")?;
      Ok(expr)
    } else if self.starts_with("{") {
      // Map or Sup
      self.parse_map_or_sup()
    } else if self.starts_with("[") {
      // List or Comprehension
      self.parse_list_or_comprehension()
    } else if self.starts_with("![") {
      // Tree Node
      self.parse_tree_node()
    } else if self.starts_with("!") {
      // Tree Leaf
      self.parse_tree_leaf(inline)
    } else if self.starts_with("`") {
      // Symbol
      Ok(Expr::Num { val: Num::U24(self.parse_quoted_symbol()?) })
    } else if self.starts_with("\"") {
      // String
      Ok(Expr::Str { val: STRINGS.get(self.parse_quoted_string()?) })
    } else if self.starts_with("'") {
      // Char
      Ok(Expr::Num { val: Num::U24(self.parse_quoted_char()? as u32 & 0x00ff_ffff) })
    } else if self.starts_with("$") {
      // Unscoped var
      self.advance_one();
      Ok(Expr::Chn { nam: self.parse_bend_name()? })
    } else if self.starts_with("*") {
      // Era
      self.advance_one();
      Ok(Expr::Era)
    } else if let Some(c) = self.peek_one() {
      if is_num_char(c) {
        // Number
        Ok(Expr::Num { val: self.parse_number()? })
      } else {
        // Var
        let nam = self.labelled(|p| p.parse_bend_name(), "expression")?;
        Ok(Expr::Var { nam })
      }
    } else {
      self.expected("expression")?
    }
  }

  fn call_or_postfix(&mut self, inline: bool) -> ParseResult<Expr> {
    let ini_idx = *self.index();
    let base = self.parse_primary_expr(inline)?;
    if inline {
      self.skip_trivia_inline()?;
    } else {
      self.skip_trivia();
    }

    // call
    if self.starts_with("(") {
      self.advance_one();
      let mut args = Vec::new();
      let mut kwargs = Vec::new();
      let mut must_be_named = false;
      while !self.starts_with(")") {
        let ini_idx = *self.index();
        let (bnd, arg) = self.parse_named_arg()?;
        let end_idx = *self.index();
        if let Some(bnd) = bnd {
          must_be_named = true;
          kwargs.push((bnd, arg));
        } else if must_be_named {
          let msg = "Positional arguments are not allowed to go after named arguments.".to_string();
          return self.with_ctx(Err(msg), ini_idx..end_idx);
        } else {
          args.push(arg);
        }
        if self.starts_with(",") {
          self.consume(",")?;
        } else {
          break;
        }
      }
      self.consume(")")?;
      if args.is_empty() && kwargs.is_empty() {
        return Ok(base);
      } else {
        return Ok(Expr::Call { fun: Box::new(base), args, kwargs });
      }
    }

    // map get
    if self.starts_with("[") {
      if let Expr::Var { nam } = base {
        self.advance_one();
        let key = self.parse_expr(inline, false)?;
        self.consume("]")?;
        return Ok(Expr::MapGet { nam, key: Box::new(key) });
      } else {
        let end_idx = *self.index();
        return self.expected_spanned("Map variable name", ini_idx..end_idx);
      }
    }

    // ctr
    if self.starts_with("{") {
      if let Expr::Var { nam } = base {
        let kwargs = self.list_like(|p| p.data_kwarg(), "{", "}", ",", true, 0)?;
        return Ok(Expr::Ctr { name: nam, args: Vec::new(), kwargs });
      } else {
        let end_idx = *self.index();
        return self.expected_spanned("Constructor name", ini_idx..end_idx);
      }
    }

    // no postfix
    Ok(base)
  }

  fn parse_map_or_sup(&mut self) -> ParseResult<Expr> {
    self.advance_one();
    // Empty map
    if self.try_consume("}") {
      return Ok(Expr::Map { entries: vec![] });
    }
    let head = self.parse_expr(false, false)?;
    self.skip_trivia();
    if self.try_consume(",") {
      self.parse_sup(head)
    } else if self.try_consume(":") {
      self.parse_map_init(head)
    } else {
      self.expected("',' or ':'")
    }
  }

  fn parse_map_init(&mut self, head: Expr) -> ParseResult<Expr> {
    let mut entries = Vec::new();
    let val = self.parse_expr(false, false)?;
    entries.push((head, val));
    self.skip_trivia();
    if !self.starts_with("}") {
      self.consume(",")?;
    }
    let tail = self.list_like(|p| p.parse_map_entry(), "", "}", ",", true, 0)?;
    entries.extend(tail);
    Ok(Expr::Map { entries })
  }

  fn parse_sup(&mut self, head: Expr) -> ParseResult<Expr> {
    let mut els = vec![head];
    let tail = self.list_like(|p| p.parse_expr(false, false), "", "}", ",", true, 1)?;
    els.extend(tail);
    Ok(Expr::Sup { els })
  }

  fn parse_tree_node(&mut self) -> ParseResult<Expr> {
    self.advance_one();
    self.advance_one();
    let left = self.parse_expr(false, false)?;
    self.consume(",")?;
    let right = self.parse_expr(false, false)?;
    self.consume("]")?;
    Ok(Expr::TreeNode { left: Box::new(left), right: Box::new(right) })
  }

  fn parse_tree_leaf(&mut self, inline: bool) -> ParseResult<Expr> {
    self.advance_one();
    let val = self.parse_expr(inline, false)?;
    Ok(Expr::TreeLeaf { val: Box::new(val) })
  }

  fn data_kwarg(&mut self) -> ParseResult<(Name, Expr)> {
    self.skip_trivia();
    let nam = self.parse_bend_name()?;
    self.consume(":")?;
    let expr = self.parse_expr(false, false)?;
    Ok((nam, expr))
  }

  fn parse_map_entry(&mut self) -> ParseResult<(Expr, Expr)> {
    let key = self.parse_expr(false, false)?;
    self.consume(":")?;
    let val = self.parse_expr(false, false)?;
    Ok((key, val))
  }

  fn parse_list_or_comprehension(&mut self) -> ParseResult<Expr> {
    self.consume_exactly("[")?;

    // Empty list
    self.skip_trivia();
    if self.try_consume_exactly("]") {
      return Ok(Expr::Lst { els: vec![] });
    }

    let head = self.parse_expr(false, false)?;
    self.skip_trivia();
    if self.try_parse_keyword("for") {
      // Comprehension
      self.skip_trivia();
      let bind = self.parse_bend_name()?;
      self.skip_trivia();
      self.parse_keyword("in")?;
      let iter = self.parse_expr(false, false)?;
      let mut cond = None;
      self.skip_trivia();
      if self.try_parse_keyword("if") {
        cond = Some(Box::new(self.parse_expr(false, false)?));
      }
      self.consume("]")?;
      Ok(Expr::LstMap { term: Box::new(head), bind, iter: Box::new(iter), cond })
    } else {
      // List
      let mut head = vec![head];
      self.skip_trivia();
      if !self.starts_with("]") {
        self.consume(",")?;
      }
      let tail = self.list_like(|p| p.parse_expr(false, false), "", "]", ",", true, 0)?;
      head.extend(tail);
      Ok(Expr::Lst { els: head })
    }
  }

  /// "λ" (<name> ","?)+ ":" <expr>
  /// | "open" <type> ":" <var>
  /// | <infix>
  fn parse_expr(&mut self, inline: bool, tup: bool) -> ParseResult<Expr> {
    if inline {
      self.skip_trivia_inline()?;
    } else {
      self.skip_trivia();
    }

    let base = self.parse_infix_expr(0, inline)?;
    if !tup {
      return Ok(base);
    }
    if inline {
      self.skip_trivia_inline()?;
    } else {
      self.skip_trivia();
    }
    if self.starts_with(",") {
      let mut els = vec![base];
      loop {
        if self.starts_with(",") {
          self.advance_one();
          els.push(self.parse_expr(inline, false)?);
          if self.starts_with("\n") {
            break;
          }
          if inline {
            self.skip_trivia_inline()?;
          } else {
            self.skip_trivia();
          }
        } else {
          break;
        }
      }
      Ok(Expr::Tup { els })
    } else {
      Ok(base)
    }
  }

  /// Named argument of a function call.
  fn parse_named_arg(&mut self) -> ParseResult<(Option<Name>, Expr)> {
    let arg = self.parse_expr(false, false)?;
    if self.try_consume("=") {
      if let Expr::Var { nam } = arg {
        let bind = Some(nam);
        let arg = self.parse_expr(false, false)?;
        Ok((bind, arg))
      } else {
        let msg = "Unexpected '=' in unnamed argument.".to_string();
        let idx = *self.index();
        self.with_ctx(Err(msg), idx..idx + 1)
      }
    } else {
      Ok((None, arg))
    }
  }

  /// Infix expression.
  /// <simple> (<infix_op> <infix>)?
  fn parse_infix_expr(&mut self, prec: usize, inline: bool) -> ParseResult<Expr> {
    maybe_grow(|| {
      if inline {
        self.skip_trivia_inline()?;
      } else {
        self.skip_trivia();
      }
      if prec > Op::max_precedence() {
        return self.call_or_postfix(inline);
      }
      let mut lhs = self.parse_infix_expr(prec + 1, inline)?;
      if inline {
        self.skip_trivia_inline()?;
      } else {
        self.skip_trivia();
      }
      while let Some(op) = self.peek_oper() {
        if op.precedence() == prec {
          self.try_parse_oper().unwrap();
          let rhs = self.parse_infix_expr(prec + 1, inline)?;
          lhs = Expr::Opr { op, lhs: Box::new(lhs), rhs: Box::new(rhs) };
          self.skip_trivia_inline()?;
        } else {
          break;
        }
      }
      Ok(lhs)
    })
  }

  fn consume_indent_at_most(&mut self, expected: Indent) -> ParseResult<Indent> {
    let got = self.advance_newlines()?;
    match (expected, got) {
      (_, Indent::Eof) => Ok(Indent::Eof),
      (Indent::Val(expected), Indent::Val(got)) if got <= expected => Ok(Indent::Val(got)),
      (expected, got) => self.expected_indent(expected, got),
    }
  }

  fn consume_indent_exactly(&mut self, expected: Indent) -> ParseResult<()> {
    let got = self.advance_newlines()?;
    match (expected, got) {
      (Indent::Eof, Indent::Eof) => Ok(()),
      (Indent::Val(expected), Indent::Val(got)) if got == expected => Ok(()),
      (expected, got) => self.expected_indent(expected, got),
    }
  }

  /// Parses a statement and returns the indentation of the next statement.
  fn parse_statement(&mut self, indent: &mut Indent) -> ParseResult<(Stmt, Indent)> {
    maybe_grow(|| {
      if self.try_parse_keyword("return") {
        self.parse_return()
      } else if self.try_parse_keyword("def") {
        self.parse_local_def(indent)
      } else if self.try_parse_keyword("if") {
        self.parse_if(indent)
      } else if self.try_parse_keyword("match") {
        self.parse_match(indent)
      } else if self.try_parse_keyword("switch") {
        self.parse_switch(indent)
      } else if self.try_parse_keyword("fold") {
        self.parse_fold(indent)
      } else if self.try_parse_keyword("bend") {
        self.parse_bend(indent)
      } else if self.try_parse_keyword("with") {
        self.parse_with(indent)
      } else if self.try_parse_keyword("open") {
        self.parse_open(indent)
      } else if self.try_parse_keyword("use") {
        self.parse_use(indent)
      } else {
        self.parse_assign(indent)
      }
    })
  }

  /// Assignments, monadic bind operations and in-place operations.
  /// <assign_pattern> "=" <expr> ";"?
  /// | <assign_pattern> "<-" <expr> ";"?
  ///
  fn parse_assign(&mut self, indent: &mut Indent) -> ParseResult<(Stmt, Indent)> {
    let ini_idx = *self.index();
    let pat = self.parse_assign_pattern()?;
    let end_idx = *self.index();
    self.skip_trivia_inline()?;

    // Assignment
    if self.starts_with("=") {
      self.advance_one();
      let val = self.parse_expr(true, true)?;
      self.skip_trivia_inline()?;
      self.try_consume_exactly(";");
      if !self.is_eof() {
        self.consume_new_line()?;
      }
      let nxt_indent = self.advance_newlines()?;
      if nxt_indent == *indent {
        let (nxt, nxt_indent) = self.parse_statement(indent)?;
        let stmt = Stmt::Assign { pat, val: Box::new(val), nxt: Some(Box::new(nxt)) };
        return Ok((stmt, nxt_indent));
      } else {
        let stmt = Stmt::Assign { pat, val: Box::new(val), nxt: None };
        return Ok((stmt, nxt_indent));
      }
    }
    // Ask
    if self.starts_with("<-") {
      self.consume("<-")?;
      let val = self.parse_expr(true, true)?;
      self.skip_trivia_inline()?;
      self.try_consume_exactly(";");
      let nxt_indent = self.advance_newlines()?;
      if nxt_indent == *indent {
        let (nxt, nxt_indent) = self.parse_statement(indent)?;
        let stmt = Stmt::Ask { pat, val: Box::new(val), nxt: Some(Box::new(nxt)) };
        return Ok((stmt, nxt_indent));
      } else {
        let stmt = Stmt::Ask { pat, val: Box::new(val), nxt: None };
        return Ok((stmt, nxt_indent));
      }
    }
    // In-place

    match &pat {
      AssignPattern::Var(..) => {}
      AssignPattern::MapSet(..) => {}
      _ => self.expected_spanned("Var or Map accessor", ini_idx..end_idx)?,
    }
    if let Some(op) = self.parse_in_place_op()? {
      let val = self.parse_expr(true, false)?;
      self.skip_trivia_inline()?;
      self.try_consume_exactly(";");
      self.consume_indent_exactly(*indent)?;
      let (nxt, nxt_indent) = self.parse_statement(indent)?;
      let stmt = Stmt::InPlace { op, pat: Box::new(pat), val: Box::new(val), nxt: Box::new(nxt) };
      return Ok((stmt, nxt_indent));
    }

    self.expected_spanned("statement", ini_idx..end_idx)
  }

  fn parse_in_place_op(&mut self) -> ParseResult<Option<InPlaceOp>> {
    self.skip_trivia_inline()?;
    let op = if self.starts_with("+=") {
      self.consume("+=")?;
      Some(InPlaceOp::Add)
    } else if self.starts_with("-=") {
      self.consume("-=")?;
      Some(InPlaceOp::Sub)
    } else if self.starts_with("*=") {
      self.consume("*=")?;
      Some(InPlaceOp::Mul)
    } else if self.starts_with("/=") {
      self.consume("/=")?;
      Some(InPlaceOp::Div)
    } else if self.starts_with("&=") {
      self.consume("&=")?;
      Some(InPlaceOp::And)
    } else if self.starts_with("|=") {
      self.consume("|=")?;
      Some(InPlaceOp::Or)
    } else if self.starts_with("^=") {
      self.consume("^=")?;
      Some(InPlaceOp::Xor)
    } else if self.starts_with("@=") {
      self.consume("@=")?;
      Some(InPlaceOp::Map)
    } else {
      None
    };
    Ok(op)
  }

  fn parse_return(&mut self) -> ParseResult<(Stmt, Indent)> {
    let term = self.parse_expr(true, true)?;
    self.skip_trivia_inline()?;
    self.try_consume_exactly(";");
    if !self.is_eof() {
      self.consume_new_line()?;
    }
    let indent = self.advance_newlines()?;
    Ok((Stmt::Return { term: Box::new(term) }, indent))
  }

  fn parse_if(&mut self, indent: &mut Indent) -> ParseResult<(Stmt, Indent)> {
    let cond = self.parse_expr(true, false)?;
    self.skip_trivia_inline()?;
    self.consume_exactly(":")?;
    indent.enter_level();

    self.consume_indent_exactly(*indent)?;
    let (then, nxt_indent) = self.parse_statement(indent)?;
    indent.exit_level();

    if nxt_indent != *indent {
      return self
        .expected_indent(*indent, nxt_indent)
        .or(self.expected_spanned("'else' or 'elif'", self.index..self.index + 1));
    }
    let mut elifs = Vec::new();
    while self.try_parse_keyword("elif") {
      let cond = self.parse_expr(true, false)?;
      self.skip_trivia_inline()?;
      self.consume_exactly(":")?;
      indent.enter_level();
      self.consume_indent_exactly(*indent)?;
      let (then, nxt_indent) = self.parse_statement(indent)?;
      indent.exit_level();

      if nxt_indent != *indent {
        return self
          .expected_indent(*indent, nxt_indent)
          .or(self.expected_spanned("'else' or 'elif'", self.index..self.index + 1));
      }
      elifs.push((cond, then));
    }
    self.parse_keyword("else")?;
    self.skip_trivia_inline()?;
    self.consume_exactly(":")?;
    indent.enter_level();

    self.consume_indent_exactly(*indent)?;
    let (otherwise, nxt_indent) = self.parse_statement(indent)?;
    let otherwise = elifs.into_iter().rfold(otherwise, |acc, (cond, then)| Stmt::If {
      cond: Box::new(cond),
      then: Box::new(then),
      otherwise: Box::new(acc),
      nxt: None,
    });

    indent.exit_level();
    if nxt_indent == *indent {
      let (nxt, nxt_indent) = self.parse_statement(indent)?;
      let stmt = Stmt::If {
        cond: Box::new(cond),
        then: Box::new(then),
        otherwise: Box::new(otherwise),
        nxt: Some(Box::new(nxt)),
      };
      Ok((stmt, nxt_indent))
    } else {
      let stmt =
        Stmt::If { cond: Box::new(cond), then: Box::new(then), otherwise: Box::new(otherwise), nxt: None };
      Ok((stmt, nxt_indent))
    }
  }

  fn parse_match(&mut self, indent: &mut Indent) -> ParseResult<(Stmt, Indent)> {
    let (bnd, arg) = self.parse_match_arg()?;
    self.skip_trivia_inline()?;
    let (with_bnd, with_arg) = self.parse_with_clause()?;
    self.consume_new_line()?;
    indent.enter_level();

    self.consume_indent_exactly(*indent).or(self.expected_spanned("'case'", self.index..self.index + 1))?;
    let (case, mut nxt_indent) = self.parse_match_case(indent)?;
    let mut arms = vec![case];
    while nxt_indent == *indent {
      let (case, nxt_indent_) = self.parse_match_case(indent)?;
      nxt_indent = nxt_indent_;
      arms.push(case);
    }
    indent.exit_level();
    if nxt_indent == *indent {
      let (nxt, nxt_indent) = self.parse_statement(indent)?;
      let stmt = Stmt::Match { arg: Box::new(arg), bnd, with_bnd, with_arg, arms, nxt: Some(Box::new(nxt)) };
      Ok((stmt, nxt_indent))
    } else {
      let stmt = Stmt::Match { arg: Box::new(arg), bnd, with_bnd, with_arg, arms, nxt: None };
      Ok((stmt, nxt_indent))
    }
  }

  fn parse_match_arg(&mut self) -> ParseResult<(Option<Name>, Expr)> {
    let ini_idx = *self.index();
    let arg = self.parse_expr(true, false)?;
    let end_idx = *self.index();

    self.skip_trivia_inline()?;
    match (arg, self.starts_with("=")) {
      (Expr::Var { nam }, true) => {
        self.advance_one();
        Ok((Some(nam), self.parse_expr(true, false)?))
      }
      (_, true) => self.expected_spanned("argument name", ini_idx..end_idx),
      (Expr::Var { nam }, false) => Ok((Some(nam.clone()), Expr::Var { nam })),
      (arg, false) => Ok((Some(Name::new("%arg")), arg)),
    }
  }

  fn parse_with_clause(&mut self) -> ParseResult<(Vec<Option<Name>>, Vec<Expr>)> {
    self.skip_trivia_inline()?;
    let res = if self.try_parse_keyword("with") {
      self.list_like(|p| p.parse_with_arg(), "", ":", ",", true, 1)?.into_iter().unzip()
    } else {
      self.consume_exactly(":")?;
      (vec![], vec![])
    };
    Ok(res)
  }

  fn parse_with_arg(&mut self) -> ParseResult<(Option<Name>, Expr)> {
    let bind = self.parse_bend_name()?;
    self.skip_trivia_inline()?;
    if self.try_consume("=") {
      let arg = self.parse_expr(false, false)?;
      Ok((Some(bind), arg))
    } else {
      Ok((Some(bind.clone()), Expr::Var { nam: bind }))
    }
  }

  fn parse_match_case(&mut self, indent: &mut Indent) -> ParseResult<(MatchArm, Indent)> {
    self.parse_keyword("case")?;
    self.skip_trivia_inline()?;
    let pat = if self.try_consume_exactly("_") {
      None
    } else {
      let nam = self.labelled(|p| p.parse_bend_name(), "name or '_'")?;
      Some(nam)
    };
    self.skip_trivia_inline()?;
    self.consume_exactly(":")?;
    self.consume_new_line()?;
    indent.enter_level();

    self.consume_indent_exactly(*indent)?;
    let (body, nxt_indent) = self.parse_statement(indent)?;
    indent.exit_level();

    let stmt = MatchArm { lft: pat, rgt: body };
    Ok((stmt, nxt_indent))
  }

  fn parse_switch(&mut self, indent: &mut Indent) -> ParseResult<(Stmt, Indent)> {
    let (bnd, arg) = self.parse_match_arg()?;
    self.skip_trivia_inline()?;
    let (with_bnd, with_arg) = self.parse_with_clause()?;
    indent.enter_level();

    self.consume_indent_exactly(*indent)?;
    let ini_idx = *self.index();
    let (fst_case, fst_stmt, mut nxt_indent) = self.parse_switch_case(indent)?;
    let end_idx = *self.index();
    if fst_case != Some(0) {
      return self.expected_spanned("case 0", ini_idx..end_idx);
    }
    let mut arms = vec![fst_stmt];
    let mut should_continue = fst_case == Some(0);
    let mut expected_num = 1;
    while should_continue {
      if nxt_indent != *indent {
        return self
          .expected_indent(*indent, nxt_indent)
          .or(self.expected_spanned("'case'", self.index..self.index + 1));
      }
      let (case, stmt, nxt_indent_) = self.parse_switch_case(indent)?;
      nxt_indent = nxt_indent_;
      if let Some(case) = case {
        if case != expected_num {
          return self.expected(&format!("case {}", expected_num));
        }
        should_continue = true;
        arms.push(stmt);
        expected_num += 1;
      } else {
        should_continue = false;
        arms.push(stmt);
      }
    }
    indent.exit_level();
    if nxt_indent == *indent {
      let (nxt, nxt_indent) = self.parse_statement(indent)?;
      let stmt = Stmt::Switch { arg: Box::new(arg), bnd, with_bnd, with_arg, arms, nxt: Some(Box::new(nxt)) };
      Ok((stmt, nxt_indent))
    } else {
      let stmt = Stmt::Switch { arg: Box::new(arg), bnd, with_bnd, with_arg, arms, nxt: None };
      Ok((stmt, nxt_indent))
    }
  }

  fn parse_switch_case(&mut self, indent: &mut Indent) -> ParseResult<(Option<u32>, Stmt, Indent)> {
    self.parse_keyword("case")?;
    self.skip_trivia_inline()?;
    let case = if let Some(c) = self.peek_one() {
      match c {
        '_' => {
          self.advance_one();
          None
        }
        c if c.is_ascii_digit() => Some(self.parse_u32()?),
        _ => return self.expected("number or '_'"),
      }
    } else {
      return self.expected("number or '_'")?;
    };

    self.skip_trivia_inline()?;
    self.consume_exactly(":")?;
    self.consume_new_line()?;
    indent.enter_level();
    self.consume_indent_exactly(*indent)?;
    let (stmt, nxt_indent) = self.parse_statement(indent)?;
    indent.exit_level();
    Ok((case, stmt, nxt_indent))
  }

  /// "fold" <bind> ("=" <arg>)? ":"
  ///   "case" <ctr> ":"
  ///     <case>
  ///   ...
  fn parse_fold(&mut self, indent: &mut Indent) -> ParseResult<(Stmt, Indent)> {
    // Actually identical to match, except the return
    let (bind, arg) = self.parse_match_arg()?;
    self.skip_trivia_inline()?;
    let (with_bnd, with_arg) = self.parse_with_clause()?;
    self.consume_new_line()?;
    indent.enter_level();

    self.consume_indent_exactly(*indent).or(self.expected_spanned("'case'", self.index..self.index + 1))?;
    let (case, mut nxt_indent) = self.parse_match_case(indent)?;
    let mut arms = vec![case];
    while nxt_indent == *indent {
      let (case, nxt_indent_) = self.parse_match_case(indent)?;
      nxt_indent = nxt_indent_;
      arms.push(case);
    }
    indent.exit_level();
    if nxt_indent == *indent {
      let (nxt, nxt_indent) = self.parse_statement(indent)?;
      let stmt =
        Stmt::Fold { arg: Box::new(arg), bnd: bind, arms, with_bnd, with_arg, nxt: Some(Box::new(nxt)) };
      Ok((stmt, nxt_indent))
    } else {
      let stmt = Stmt::Fold { arg: Box::new(arg), bnd: bind, arms, with_bnd, with_arg, nxt: None };
      Ok((stmt, nxt_indent))
    }
  }

  /// "bend" (<bind> "=" <init> ","?)* ":"
  ///   "when" <cond> ":"
  ///     <step>
  ///   "else" ":"
  ///     <base>
  fn parse_bend(&mut self, indent: &mut Indent) -> ParseResult<(Stmt, Indent)> {
    let args = self.list_like(|p| p.parse_match_arg(), "", ":", ",", true, 1)?;
    let (bind, init) = args.into_iter().unzip();
    self.consume_new_line()?;
    indent.enter_level();

    self.consume_indent_exactly(*indent).or(self.expected_spanned("'when'", self.index..self.index + 1))?;
    self.parse_keyword("when")?;
    let cond = self.parse_expr(true, false)?;
    self.skip_trivia_inline()?;
    self.consume_exactly(":")?;
    self.consume_new_line()?;
    indent.enter_level();

    self.consume_indent_exactly(*indent)?;
    let (step, nxt_indent) = self.parse_statement(indent)?;
    indent.exit_level();

    if nxt_indent != *indent {
      return self
        .expected_indent(*indent, nxt_indent)
        .or(self.expected_spanned("'else'", self.index..self.index + 1));
    }
    self.parse_keyword("else")?;
    self.skip_trivia_inline()?;
    self.consume_exactly(":")?;
    self.consume_new_line()?;
    indent.enter_level();

    self.consume_indent_exactly(*indent)?;
    let (base, nxt_indent) = self.parse_statement(indent)?;
    indent.exit_level();

    indent.exit_level();
    if nxt_indent == *indent {
      let (nxt, nxt_indent) = self.parse_statement(indent)?;
      let stmt = Stmt::Bend {
        bnd: bind,
        arg: init,
        cond: Box::new(cond),
        step: Box::new(step),
        base: Box::new(base),
        nxt: Some(Box::new(nxt)),
      };
      Ok((stmt, nxt_indent))
    } else {
      let stmt = Stmt::Bend {
        bnd: bind,
        arg: init,
        cond: Box::new(cond),
        step: Box::new(step),
        base: Box::new(base),
        nxt: None,
      };
      Ok((stmt, nxt_indent))
    }
  }

  /// "with" <typ> ":"
  ///   <bod>
  /// <nxt>?
  fn parse_with(&mut self, indent: &mut Indent) -> ParseResult<(Stmt, Indent)> {
    self.skip_trivia_inline()?;
    let typ = self.parse_bend_name()?;
    self.skip_trivia_inline()?;
    self.consume_exactly(":")?;
    self.consume_new_line()?;
    indent.enter_level();

    self.consume_indent_exactly(*indent)?;
    let (bod, nxt_indent) = self.parse_statement(indent)?;
    indent.exit_level();

    if nxt_indent == *indent {
      let (nxt, nxt_indent) = self.parse_statement(indent)?;
      let stmt = Stmt::With { typ, bod: Box::new(bod), nxt: Some(Box::new(nxt)) };
      Ok((stmt, nxt_indent))
    } else {
      let stmt = Stmt::With { typ, bod: Box::new(bod), nxt: None };
      Ok((stmt, nxt_indent))
    }
  }

  /// <pat1>
  /// | <nam> "[" <expr> "]"
  /// | <pat1> ("," <pat1>)*
  fn parse_assign_pattern(&mut self) -> ParseResult<AssignPattern> {
    let head_ini = *self.index();
    let head = self.parse_primary_assign_pattern()?;
    let head_end = *self.index();
    self.skip_trivia_inline()?;
    if self.starts_with("[") {
      // TODO: allow patterns like `x[a][b]`
      self.advance_one();
      let key = self.parse_expr(false, false)?;
      self.consume("]")?;
      if let AssignPattern::Var(var) = head {
        Ok(AssignPattern::MapSet(var, key))
      } else {
        self.with_ctx(Err("Expected a variable pattern"), head_ini..head_end)
      }
    } else if self.starts_with(",") {
      let mut els = vec![head];
      while self.try_consume(",") {
        self.skip_trivia_inline()?;
        els.push(self.parse_primary_assign_pattern()?);
      }
      Ok(AssignPattern::Tup(els))
    } else {
      Ok(head)
    }
  }

  /// "*"
  /// | "{"<pat1> ("," <pat1>)+ "}"
  /// | "$" <nam>
  /// | "(" <pat0> ")"
  /// | <nam>
  fn parse_primary_assign_pattern(&mut self) -> ParseResult<AssignPattern> {
    if self.starts_with("*") {
      self.advance_one();
      Ok(AssignPattern::Eraser)
    } else if self.starts_with("{") {
      let binds = self.list_like(|p| p.parse_primary_assign_pattern(), "{", "}", ",", true, 2)?;
      Ok(AssignPattern::Sup(binds))
    } else if self.starts_with("$") {
      self.advance_one();
      self.skip_trivia_inline()?;
      let nam = self.parse_bend_name()?;
      Ok(AssignPattern::Chn(nam))
    } else if self.starts_with("(") {
      self.advance_one();
      let assign = self.parse_assign_pattern()?;
      self.consume(")")?;
      Ok(assign)
    } else {
      Ok(AssignPattern::Var(self.parse_bend_name()?))
    }
  }

  /// "open" {typ} ":" {var} ";"? {nxt}
  fn parse_open(&mut self, indent: &mut Indent) -> ParseResult<(Stmt, Indent)> {
    self.skip_trivia_inline()?;
    let typ = self.labelled(|p| p.parse_bend_name(), "type name")?;
    self.skip_trivia_inline()?;
    self.consume_exactly(":")?;
    self.skip_trivia_inline()?;
    let var = self.labelled(|p| p.parse_bend_name(), "variable name")?;
    self.skip_trivia_inline()?;
    self.try_consume_exactly(";");
    self.consume_new_line()?;
    self.consume_indent_exactly(*indent)?;
    let (nxt, nxt_indent) = self.parse_statement(indent)?;
    let stmt = Stmt::Open { typ, var, nxt: Box::new(nxt) };
    Ok((stmt, nxt_indent))
  }

  fn parse_use(&mut self, indent: &mut Indent) -> ParseResult<(Stmt, Indent)> {
    self.skip_trivia_inline()?;
    let nam = self.parse_bend_name()?;
    self.skip_trivia_inline()?;
    self.consume_exactly("=")?;
    self.skip_trivia_inline()?;
    let bod = self.parse_expr(true, true)?;
    self.skip_trivia_inline()?;
    self.try_consume_exactly(";");
    self.consume_new_line()?;
    self.consume_indent_exactly(*indent)?;
    let (nxt, nxt_indent) = self.parse_statement(indent)?;
    let stmt = Stmt::Use { nam, val: Box::new(bod), nxt: Box::new(nxt) };
    Ok((stmt, nxt_indent))
  }

  fn parse_local_def(&mut self, indent: &mut Indent) -> ParseResult<(Stmt, Indent)> {
    let (def, mut nxt_indent) = self.parse_def_aux(*indent)?;
    let (nxt, nxt_indent) = self.parse_statement(&mut nxt_indent)?;
    let stmt = Stmt::LocalDef { def: Box::new(def), nxt: Box::new(nxt) };
    Ok((stmt, nxt_indent))
  }

  pub fn parse_def(&mut self, indent: Indent) -> ParseResult<(Definition, Indent)> {
    if indent != Indent::Val(0) {
      let msg = "Indentation error. Functions defined with 'def' must be at the start of the line.";
      let idx = *self.index();
      return self.with_ctx(Err(msg), idx..idx + 1);
    }
    self.parse_def_aux(indent)
  }

  fn parse_def_aux(&mut self, mut indent: Indent) -> ParseResult<(Definition, Indent)> {
    self.skip_trivia_inline()?;
    let name = self.parse_top_level_name()?;
    self.skip_trivia_inline()?;
    let params = if self.starts_with("(") {
      self.list_like(|p| p.parse_bend_name(), "(", ")", ",", true, 0)?
    } else {
      vec![]
    };
    self.skip_trivia_inline()?;
    self.consume_exactly(":")?;
    self.consume_new_line()?;
    indent.enter_level();

    self.consume_indent_exactly(indent)?;
    let (body, nxt_indent) = self.parse_statement(&mut indent)?;
    indent.exit_level();

    // Temporary source, should be overwritten later
    let source = Source { file: None, span: None, kind: SourceKind::Generated };
    let def = Definition { name, params, body, source };
    Ok((def, nxt_indent))
  }

  pub fn parse_type(&mut self, mut indent: Indent) -> ParseResult<(Enum, Indent)> {
    if indent != Indent::Val(0) {
      let msg = "Indentation error. Types defined with 'type' must be at the start of the line.";
      let idx = *self.index();
      return self.with_ctx(Err(msg), idx..idx + 1);
    }

    self.skip_trivia_inline()?;
    let typ_name = self.parse_top_level_name()?;
    self.skip_trivia_inline()?;
    self.consume_exactly(":")?;
    self.consume_new_line()?;
    indent.enter_level();

    self.consume_indent_exactly(indent)?;
    let mut variants = Vec::new();
    let mut nxt_indent = indent;
    while nxt_indent == indent {
      variants.push(self.parse_enum_variant(&typ_name)?);
      if !self.is_eof() {
        self.consume_new_line()?;
      }
      nxt_indent = self.consume_indent_at_most(indent)?;
    }
    indent.exit_level();

    let enum_ = Enum { name: typ_name, variants };
    Ok((enum_, nxt_indent))
  }

  pub fn parse_enum_variant(&mut self, typ_name: &Name) -> ParseResult<Variant> {
    let ctr_name = self.parse_top_level_name()?;
    let ctr_name = Name::new(format!("{typ_name}/{ctr_name}"));
    let mut fields = Vec::new();
    self.skip_trivia_inline()?;
    if self.starts_with("{") {
      fields = self.list_like(|p| p.parse_variant_field(), "{", "}", ",", true, 0)?;
    }
    if let Some(field) = fields.find_repeated_names().into_iter().next() {
      let msg = format!("Found a repeated field '{field}' in constructor {ctr_name}.");
      return self.expected_message(&msg);
    }
    Ok(Variant { name: ctr_name, fields })
  }

  pub fn parse_object(&mut self, indent: Indent) -> ParseResult<(Variant, Indent)> {
    if indent != Indent::Val(0) {
      let msg = "Indentation error. Types defined with 'object' must be at the start of the line.";
      let idx = *self.index();
      return self.with_ctx(Err(msg), idx..idx + 1);
    }

    self.skip_trivia_inline()?;
    let name = self.parse_top_level_name()?;
    self.skip_trivia_inline()?;
    let fields = if self.starts_with("{") {
      self.list_like(|p| p.parse_variant_field(), "{", "}", ",", true, 0)?
    } else {
      vec![]
    };
    if let Some(field) = fields.find_repeated_names().into_iter().next() {
      let msg = format!("Found a repeated field '{field}' in object {name}.");
      return self.expected_message(&msg);
    }
    if !self.is_eof() {
      self.consume_new_line()?;
    }
    let nxt_indent = self.advance_newlines()?;
    Ok((Variant { name, fields }, nxt_indent))
  }

  fn parse_variant_field(&mut self) -> ParseResult<CtrField> {
    let rec = self.try_consume_exactly("~");
    self.skip_trivia();
    let nam = self.parse_bend_name()?;
    Ok(CtrField { nam, rec })
  }

  fn expected_indent<T>(&mut self, expected: Indent, got: Indent) -> ParseResult<T> {
    match (expected, got) {
      (Indent::Eof, Indent::Eof) => unreachable!(),
      (Indent::Eof, Indent::Val(got)) => {
        let msg = format!("Indentation error. Expected end-of-input, got {} spaces.", got);
        let idx = *self.index();
        self.with_ctx(Err(msg), idx..idx + 1)
      }
      (Indent::Val(expected), Indent::Eof) => {
        let msg = format!("Indentation error. Expected {} spaces, got end-of-input.", expected);
        let idx = *self.index();
        self.with_ctx(Err(msg), idx..idx + 1)
      }
      (Indent::Val(expected), Indent::Val(got)) => {
        if got != expected {
          let msg = format!("Indentation error. Expected {} spaces, got {}.", expected, got);
          let idx = *self.index();
          self.with_ctx(Err(msg), idx..idx + 1)
        } else {
          unreachable!()
        }
      }
    }
  }
}

impl Op {
  fn precedence(&self) -> usize {
    match self {
      Op::OR => 0,
      Op::XOR => 1,
      Op::AND => 2,
      Op::EQ => 3,
      Op::NEQ => 3,
      Op::LT => 4,
      Op::GT => 4,
      Op::LE => 4,
      Op::GE => 4,
      Op::SHL => 5,
      Op::SHR => 5,
      Op::ADD => 6,
      Op::SUB => 6,
      Op::MUL => 7,
      Op::DIV => 7,
      Op::REM => 7,
      Op::POW => 8,
      Op::ATN => todo!(),
      Op::LOG => todo!(),
    }
  }
  fn max_precedence() -> usize {
    8
  }
}
