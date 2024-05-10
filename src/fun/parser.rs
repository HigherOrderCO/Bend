use crate::{
  fun::{
    display::DisplayFn, Adt, Book, Definition, FanKind, MatchRule, Name, Num, Op, Pattern, Rule, Tag, Term,
    STRINGS,
  },
  maybe_grow,
};
use highlight_error::highlight_error;
use TSPL::Parser;

use super::CtrField;

// Bend grammar description:
// <Book>       ::= (<Data> | <Rule>)*
// <Data>       ::= "data" <Name> "=" ( <Name> | "(" <Name> (<Name>)* ")" )+
// <Rule>       ::= ("(" <Name> <Pattern>* ")" | <Name> <Pattern>*) "=" <Term>
// <Pattern>    ::= "(" <Name> <Pattern>* ")" | <NameEra> | <Number> | "(" <Pattern> ("," <Pattern>)+ ")"
// <Term>       ::=
//   <Number> | <NumOp> | <Tup> | <App> | <Group> | <Nat> | <Lam> | <UnscopedLam> | <Bend> | <Fold> |
//   <Use> | <Dup> | <LetTup> | <Let> | <Bind> | <Match> | <Switch> | <Era> | <UnscopedVar> | <Var>
// <Lam>        ::= <Tag>? ("λ"|"@") <NameEra> <Term>
// <UnscopedLam>::= <Tag>? ("λ"|"@") "$" <Name> <Term>
// <NumOp>      ::= "(" <Operator> <Term> <Term> ")"
// <Tup>        ::= "(" <Term> ("," <Term>)+ ")"
// <App>        ::= <Tag>? "(" <Term> (<Term>)+ ")"
// <Group>      ::= "(" <Term> ")"
// <Use>        ::= "use" <Name> "=" <Term> ";"? <Term>
// <Let>        ::= "let" <NameEra> "=" <Term> ";"? <Term>
// <Bind>       ::= "do" <Name> "{" <Ask> "}"
// <Ask>        ::= "ask" <Pattern> "=" <Term> ";" <Term> | <Term>
// <LetTup>     ::= "let" "(" <NameEra> ("," <NameEra>)+ ")" "=" <Term> ";"? <Term>
// <Dup>        ::= "let" <Tag>? "{" <NameEra> (","? <NameEra>)+ "}" "=" <Term> ";"? <Term>
// <List>       ::= "[" (<Term> ","?)* "]"
// <String>     ::= "\"" (escape sequence | [^"])* "\""
// <Char>       ::= "'" (escape sequence | [^']) "'"
// <Match>      ::= "match" <Name> ("=" <Term>)? ("with" <Var> (","? <Var>)*)? "{" <MatchArm>+ "}"
// <MatchArm>   ::= "|"? <Pattern> ":" <Term> ";"?
// <Switch>     ::= "switch" <Name> ("=" <Term>)? ("with" <Var> (","? <Var>)*)? "{" <SwitchArm>+ "}"
// <SwitchArm>  ::= "|"? (<Num>|"_") ":" <Term> ";"?
// <Var>        ::= <Name>
// <UnscopedVar>::= "$" <Name>
// <NameEra>    ::= <Name> | "*"
// <Era>        ::= "*"
// <Tag>        ::= "#" <Name>
// <Name>       ::= [_\-./a-zA-Z0-9]+
// <Number>     ::= ([0-9]+ | "0x"[0-9a-fA-F]+ | "0b"[01]+)
// <Operator>   ::= ( "+" | "-" | "*" | "/" | "%" | "==" | "!=" | "<<" | ">>" | "<=" | ">=" | "<" | ">" | "^" )

pub struct TermParser<'i> {
  input: &'i str,
  index: usize,
}

impl<'a> TermParser<'a> {
  pub fn new(input: &'a str) -> Self {
    Self { input, index: 0 }
  }

  /* AST parsing functions */

  pub fn parse_book(&mut self, default_book: Book, builtin: bool) -> Result<Book, String> {
    let mut book = default_book;
    self.skip_trivia();
    while !self.is_eof() {
      let ini_idx = *self.index();
      if self.skip_starts_with("data") {
        // adt declaration
        let (nam, adt) = self.parse_datatype(builtin)?;
        let end_idx = *self.index();
        self.with_ctx(book.add_adt(nam, adt), ini_idx, end_idx)?;
      } else {
        // function declaration rule
        let (name, rule) = self.parse_rule()?;
        book.add_rule(name, rule, builtin);
      }
      self.skip_trivia();
    }

    Ok(book)
  }

  fn parse_datatype(&mut self, builtin: bool) -> Result<(Name, Adt), String> {
    // data name = ctr (| ctr)*
    self.consume("data")?;
    let name = self.labelled(|p| p.parse_top_level_name(), "datatype name")?;
    self.consume("=")?;
    let mut ctrs = vec![self.parse_datatype_ctr()?];
    while self.try_consume("|") {
      ctrs.push(self.parse_datatype_ctr()?);
    }
    let ctrs = ctrs.into_iter().collect();
    let adt = Adt { ctrs, builtin };
    Ok((name, adt))
  }

  fn parse_datatype_ctr(&mut self) -> Result<(Name, Vec<CtrField>), String> {
    // (name  ('~'? field)*)
    // name
    if self.try_consume("(") {
      let name = self.parse_top_level_name()?;

      fn parse_field(p: &mut TermParser) -> Result<CtrField, String> {
        let rec = p.try_consume("~");
        let nam = p.labelled(|p| p.parse_bend_name(), "datatype constructor field")?;
        Ok(CtrField { nam, rec })
      }

      let fields = self.list_like(parse_field, "", ")", "", false, 0)?;
      Ok((name, fields))
    } else {
      // name
      let name = self.labelled(|p| p.parse_top_level_name(), "datatype constructor name")?;
      Ok((name, vec![]))
    }
  }

  fn parse_rule(&mut self) -> Result<(Name, Rule), String> {
    // (name pat*) = term
    // name pat* = term
    let (name, pats) = if self.try_consume("(") {
      let name = self.labelled(|p| p.parse_top_level_name(), "function name")?;
      let pats = self.list_like(|p| p.parse_pattern(false), "", ")", "", false, 0)?;
      (name, pats)
    } else {
      let name = self.labelled(|p| p.parse_top_level_name(), "top-level definition")?;
      let mut pats = vec![];
      while !self.skip_starts_with("=") {
        pats.push(self.parse_pattern(false)?);
      }
      (name, pats)
    };

    self.consume("=")?;

    let body = self.parse_term()?;

    let rule = Rule { pats, body };
    Ok((name, rule))
  }

  fn parse_pattern(&mut self, simple: bool) -> Result<Pattern, String> {
    maybe_grow(|| {
      let (tag, unexpected_tag) = self.parse_tag()?;
      self.skip_trivia();

      // Ctr or Tup
      if self.starts_with("(") {
        self.consume("(")?;
        let head_ini_idx = *self.index();
        let head = self.parse_pattern(simple)?;
        let head_end_idx = *self.index();

        // Tup
        if simple || self.skip_starts_with(",") {
          self.consume(",")?;
          let mut els = self.list_like(|p| p.parse_pattern(simple), "", ")", ",", true, 1)?;
          els.insert(0, head);
          return Ok(Pattern::Fan(FanKind::Tup, tag.unwrap_or(Tag::Static), els));
        }

        // Ctr
        unexpected_tag(self)?;
        let Pattern::Var(Some(name)) = head else {
          return self.expected_spanned("constructor name", head_ini_idx, head_end_idx);
        };
        let els = self.list_like(|p| p.parse_pattern(simple), "", ")", "", false, 0)?;
        return Ok(Pattern::Ctr(name, els));
      }

      // Dup
      if self.starts_with("{") {
        let els = self.list_like(|p| p.parse_pattern(simple), "{", "}", ",", false, 0)?;
        return Ok(Pattern::Fan(FanKind::Dup, tag.unwrap_or(Tag::Auto), els));
      }

      // List
      if self.starts_with("[") && !simple {
        unexpected_tag(self)?;
        let els = self.list_like(|p| p.parse_pattern(simple), "[", "]", ",", false, 0)?;
        return Ok(Pattern::Lst(els));
      }

      // String
      if self.starts_with("\"") && !simple {
        unexpected_tag(self)?;
        let str = self.parse_quoted_string()?;
        return Ok(Pattern::Str(STRINGS.get(str)));
      }

      // Char
      if self.starts_with("'") {
        unexpected_tag(self)?;
        let char = self.parse_quoted_char()?;
        return Ok(Pattern::Num(char as u32));
      }

      // Number
      if self.peek_one().map_or(false, |c| c.is_ascii_digit()) {
        unexpected_tag(self)?;
        let num = self.parse_u32()?;
        return Ok(Pattern::Num(num));
      }

      // Channel
      if self.starts_with("$") {
        unexpected_tag(self)?;
        self.advance_one();
        let name = self.parse_bend_name()?;
        return Ok(Pattern::Chn(name));
      }

      // Var
      unexpected_tag(self)?;
      let nam = self.labelled(|p| p.parse_name_or_era(), "pattern-matching pattern")?;
      Ok(Pattern::Var(nam))
    })
  }

  pub fn parse_term(&mut self) -> Result<Term, String> {
    maybe_grow(|| {
      let (tag, unexpected_tag) = self.parse_tag()?;
      self.skip_trivia();

      // Lambda, unscoped lambda
      if self.starts_with("λ") || self.starts_with("@") {
        self.advance_one().unwrap();
        let tag = tag.unwrap_or(Tag::Static);
        let pat = self.parse_pattern(true)?;
        let bod = self.parse_term()?;
        return Ok(Term::Lam { tag, pat: Box::new(pat), bod: Box::new(bod) });
      }

      // App, Tup, Num Op
      if self.starts_with("(") {
        self.consume("(")?;

        // Opr but maybe a tup
        let starts_with_oper = self.skip_peek_one().map_or(false, |c| "+-*/%&|<>^=!".contains(c));
        if starts_with_oper {
          let opr = self.parse_oper()?;

          // jk, actually a tuple
          if self.skip_starts_with(",") && opr == Op::MUL {
            let mut els = vec![Term::Era];
            while self.try_consume(",") {
              els.push(self.parse_term()?);
            }
            self.consume(")")?;
            return Ok(Term::Fan { fan: FanKind::Tup, tag: tag.unwrap_or(Tag::Static), els });
          }

          // Opr
          unexpected_tag(self)?;
          let fst = self.parse_term()?;
          let snd = self.parse_term()?;
          self.consume(")")?;
          return Ok(Term::Oper { opr, fst: Box::new(fst), snd: Box::new(snd) });
        }

        // Tup or App
        let head = self.parse_term()?;

        // Tup
        if self.skip_starts_with(",") {
          let mut els = vec![head];
          while self.try_consume(",") {
            els.push(self.parse_term()?);
          }
          self.consume(")")?;
          return Ok(Term::Fan { fan: FanKind::Tup, tag: tag.unwrap_or(Tag::Static), els });
        }

        // App
        let els = self.list_like(|p| p.parse_term(), "", ")", "", false, 0)?;
        let term = els.into_iter().fold(head, |fun, arg| Term::App {
          tag: tag.clone().unwrap_or(Tag::Static),
          fun: Box::new(fun),
          arg: Box::new(arg),
        });
        return Ok(term);
      }

      // List
      if self.starts_with("[") {
        unexpected_tag(self)?;
        let els = self.list_like(|p| p.parse_term(), "[", "]", ",", false, 0)?;
        return Ok(Term::List { els });
      }

      // Sup
      if self.starts_with("{") {
        let els = self.list_like(|p| p.parse_term(), "{", "}", ",", false, 2)?;
        return Ok(Term::Fan { fan: FanKind::Dup, tag: tag.unwrap_or(Tag::Auto), els });
      }

      // Unscoped var
      if self.starts_with("$") {
        self.consume("$")?;
        unexpected_tag(self)?;
        let nam = self.parse_bend_name()?;
        return Ok(Term::Link { nam });
      }

      // Era
      if self.starts_with("*") {
        self.consume("*")?;
        unexpected_tag(self)?;
        return Ok(Term::Era);
      }

      // Nat
      if self.starts_with("#") {
        self.consume("#")?;
        unexpected_tag(self)?;
        let val = self.parse_u32()?;
        return Ok(Term::Nat { val });
      }

      // String
      if self.starts_with("\"") {
        unexpected_tag(self)?;
        let str = self.parse_quoted_string()?;
        return Ok(Term::Str { val: STRINGS.get(str) });
      }

      // Char
      if self.starts_with("'") {
        unexpected_tag(self)?;
        let char = self.parse_quoted_char()?;
        return Ok(Term::Num { val: Num::U24(char as u32 & 0x00ff_ffff) });
      }

      // Native Number
      if self.peek_one().map_or(false, |c| "0123456789+-".contains(c)) {
        unexpected_tag(self)?;

        let ini_idx = *self.index();

        // Parses sign
        let sgn = if self.try_consume("+") {
          Some(1)
        } else if self.try_consume("-") {
          Some(-1)
        } else {
          None
        };

        // Parses main value
        let num = self.parse_u32()?;

        // Parses frac value (Float type)
        // TODO: Will lead to some rounding errors
        // TODO: Doesn't cover very large/small numbers
        let fra = if let Some('.') = self.peek_one() {
          self.consume(".")?;
          let ini_idx = *self.index();
          let fra = self.parse_u32()? as f32;
          let end_idx = *self.index();
          let fra = fra / 10f32.powi((end_idx - ini_idx) as i32);
          Some(fra)
        } else {
          None
        };

        // F24
        if let Some(fra) = fra {
          let sgn = sgn.unwrap_or(1);
          return Ok(Term::Num { val: Num::F24(sgn as f32 * (num as f32 + fra)) });
        }

        // I24
        if let Some(sgn) = sgn {
          let num = sgn * num as i32;
          if !(-0x00800000 ..= 0x007fffff).contains(&num) {
            return self.num_range_err(ini_idx, "I24");
          }
          return Ok(Term::Num { val: Num::I24(num) });
        }

        // U24
        if num >= 1 << 24 {
          return self.num_range_err(ini_idx, "U24");
        }
        return Ok(Term::Num { val: Num::U24(num) });
      }

      // Use
      if self.try_consume_keyword("use") {
        unexpected_tag(self)?;
        let nam = self.parse_bend_name()?;
        self.consume("=")?;
        let val = self.parse_term()?;
        self.try_consume(";");
        let nxt = self.parse_term()?;
        return Ok(Term::Use { nam: Some(nam), val: Box::new(val), nxt: Box::new(nxt) });
      }

      // Let
      if self.try_consume_keyword("let") {
        unexpected_tag(self)?;
        let pat = self.parse_pattern(true)?;
        self.consume("=")?;
        let val = self.parse_term()?;
        self.try_consume(";");
        let nxt = self.parse_term()?;
        return Ok(Term::Let { pat: Box::new(pat), val: Box::new(val), nxt: Box::new(nxt) });
      }

      // If
      if self.try_consume_keyword("if") {
        let cnd = self.parse_term()?;
        self.consume("{")?;
        let thn = self.parse_term()?;
        self.consume("}")?;
        self.consume("else")?;
        self.consume("{")?;
        let els = self.parse_term()?;
        self.consume("}")?;
        return Ok(Term::Swt {
          arg: Box::new(cnd),
          bnd: Some(Name::new("%cond")),
          with: Vec::new(),
          pred: Some(Name::new("%cond-1")),
          arms: vec![els, thn],
        });
      }

      // Match
      if self.try_consume_keyword("match") {
        unexpected_tag(self)?;
        let (bnd, arg, with) = self.parse_match_header()?;
        let arms = self.list_like(|p| p.parse_match_arm(), "{", "}", ";", false, 1)?;
        return Ok(Term::Mat { arg: Box::new(arg), bnd, with, arms });
      }

      // Switch
      if self.try_consume_keyword("switch") {
        unexpected_tag(self)?;
        return self.parse_switch();
      }

      // Do
      if self.try_consume_keyword("do") {
        unexpected_tag(self)?;
        let typ = self.parse_name()?;
        self.consume("{")?;
        let ask = self.parse_ask(Name::new(typ))?;
        self.consume("}")?;
        return Ok(ask);
      }

      // Fold
      if self.try_consume_keyword("fold") {
        unexpected_tag(self)?;
        let (bnd, arg, with) = self.parse_match_header()?;
        let arms = self.list_like(|p| p.parse_match_arm(), "{", "}", ";", false, 1)?;
        return Ok(Term::Fold { arg: Box::new(arg), bnd, with, arms });
      }

      // Bend
      if self.try_consume_keyword("bend") {
        unexpected_tag(self)?;
        let args = self.list_like(
          |p| {
            let bind = p.parse_bend_name()?;
            let init = if p.try_consume("=") { p.parse_term()? } else { Term::Var { nam: bind.clone() } };
            Ok((bind, init))
          },
          "",
          "while",
          ",",
          false,
          0,
        )?;
        let (bind, init): (Vec<_>, Vec<_>) = args.into_iter().unzip();
        let bind = bind.into_iter().map(Some).collect::<Vec<_>>();
        let cond = self.parse_term()?;
        self.consume("{")?;
        let step = self.parse_term()?;
        self.consume("}")?;
        self.consume("then")?;
        self.consume("{")?;
        let base = self.parse_term()?;
        self.consume("}")?;
        return Ok(Term::Bend {
          bind,
          init,
          cond: Box::new(cond),
          step: Box::new(step),
          base: Box::new(base),
        });
      }

      // Var
      unexpected_tag(self)?;
      let nam = self.labelled(|p| p.parse_bend_name(), "term")?;
      Ok(Term::Var { nam })
    })
  }

  fn parse_ask(&mut self, typ: Name) -> Result<Term, String> {
    maybe_grow(|| {
      if self.try_consume_keyword("ask") {
        let ask = self.parse_pattern(true)?;
        self.consume("=")?;
        let val = self.parse_term()?;
        self.try_consume(";");
        let nxt = self.parse_ask(typ.clone())?;
        Ok(Term::Bind { typ, ask: Box::new(ask), val: Box::new(val), nxt: Box::new(nxt) })
      } else {
        self.parse_term()
      }
    })
  }

  fn parse_top_level_name(&mut self) -> Result<Name, String> {
    let ini_idx = *self.index();
    let nam = self.parse_bend_name()?;
    let end_idx = *self.index();
    if nam.contains("__") {
      let msg = "Top-level names are not allowed to contain \"__\".".to_string();
      self.with_ctx(Err(msg), ini_idx, end_idx)
    } else {
      Ok(nam)
    }
  }

  fn parse_name_or_era(&mut self) -> Result<Option<Name>, String> {
    self.labelled(
      |p| {
        if p.try_consume("*") {
          Ok(None)
        } else {
          let nam = p.parse_bend_name()?;
          Ok(Some(nam))
        }
      },
      "name or '*'",
    )
  }

  /// Parses a tag where it may or may not be valid.
  ///
  /// If it is not valid, the returned callback can be used to issue an error.
  fn parse_tag(&mut self) -> Result<(Option<Tag>, impl FnOnce(&mut Self) -> Result<(), String>), String> {
    let index = self.index;
    let tag = if self.skip_peek_one() == Some('#')
      && !self.peek_many(2).is_some_and(|x| x.chars().nth(1).unwrap().is_ascii_digit())
    {
      let msg = "Tagged terms not supported for hvm32.".to_string();
      return self.with_ctx(Err(msg), index, index + 1);
    } else {
      None
    };
    let end_index = self.index;
    let has_tag = tag.is_some();
    Ok((tag, move |slf: &mut Self| {
      if has_tag {
        let msg = "\x1b[1m- unexpected tag:\x1b[0m".to_string();
        slf.with_ctx(Err(msg), index, end_index)
      } else {
        Ok(())
      }
    }))
  }

  fn parse_match_arg(&mut self) -> Result<(Option<Name>, Term), String> {
    let ini_idx = *self.index();
    let mut arg = self.parse_term()?;
    let end_idx = *self.index();

    self.skip_trivia();
    match (&mut arg, self.starts_with("=")) {
      (Term::Var { nam }, true) => {
        self.consume("=")?;
        Ok((Some(std::mem::take(nam)), self.parse_term()?))
      }
      (Term::Var { nam }, false) => Ok((Some(nam.clone()), Term::Var { nam: std::mem::take(nam) })),
      (_, true) => self.expected_spanned("argument name", ini_idx, end_idx),
      (arg, false) => Ok((Some(Name::new("%arg")), std::mem::take(arg))),
    }
  }

  fn parse_match_header(&mut self) -> Result<(Option<Name>, Term, Vec<Name>), String> {
    let (bnd, arg) = self.parse_match_arg()?;
    let with = if self.try_consume_keyword("with") {
      let mut with = vec![self.parse_bend_name()?];
      while !self.skip_starts_with("{") {
        self.try_consume(",");
        with.push(self.parse_bend_name()?);
      }
      with
    } else {
      vec![]
    };
    Ok((bnd, arg, with))
  }

  fn parse_match_arm(&mut self) -> Result<MatchRule, String> {
    self.try_consume("|");
    let nam = self.parse_name_or_era()?;
    self.consume(":")?;
    let bod = self.parse_term()?;
    Ok((nam, vec![], bod))
  }

  fn parse_switch(&mut self) -> Result<Term, String> {
    let (bnd, arg, with) = self.parse_match_header()?;
    self.consume("{")?;
    let mut expected_num = 0;
    let mut arms = vec![];
    let mut to_continue = true;
    while to_continue && !self.skip_starts_with("}") {
      self.try_consume("|");
      let Some(head) = self.skip_peek_one() else { return self.expected("switch pattern") };
      match head {
        '_' => {
          if expected_num == 0 {
            return self.expected("0");
          } else {
            self.consume("_")?;
            to_continue = false;
          }
        }
        c if c.is_ascii_digit() => {
          let val = self.parse_u32()?;
          if val != expected_num {
            return self.expected(&expected_num.to_string());
          }
        }
        _ => return self.expected("switch pattern"),
      };
      self.consume(":")?;
      arms.push(self.parse_term()?);
      self.try_consume(";");
      expected_num += 1;
    }
    let pred = Some(Name::new(format!("{}-{}", bnd.as_ref().unwrap(), arms.len() - 1)));
    self.consume("}")?;
    Ok(Term::Swt { arg: Box::new(arg), bnd, with, pred, arms })
  }

  fn num_range_err<T>(&mut self, ini_idx: usize, typ: &str) -> Result<T, String> {
    let msg = format!("\x1b[1mNumber literal outside of range for {}.\x1b[0m", typ);
    let end_idx = *self.index();
    self.with_ctx(Err(msg), ini_idx, end_idx)
  }
}

impl<'a> Parser<'a> for TermParser<'a> {
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

fn is_name_char(c: char) -> bool {
  c.is_ascii_alphanumeric() || c == '_' || c == '.' || c == '-' || c == '/'
}

impl Book {
  fn add_adt(&mut self, nam: Name, adt: Adt) -> Result<(), String> {
    if let Some(adt) = self.adts.get(&nam) {
      if adt.builtin {
        return Err(format!("{} is a built-in datatype and should not be overridden.", nam));
      } else {
        return Err(format!("Repeated datatype '{}'", nam));
      }
    } else {
      for ctr in adt.ctrs.keys() {
        match self.ctrs.entry(ctr.clone()) {
          indexmap::map::Entry::Vacant(e) => _ = e.insert(nam.clone()),
          indexmap::map::Entry::Occupied(e) => {
            if self.adts.get(e.get()).is_some_and(|adt| adt.builtin) {
              return Err(format!("{} is a built-in constructor and should not be overridden.", e.key()));
            } else {
              return Err(format!("Repeated constructor '{}'", e.key()));
            }
          }
        }
      }
      self.adts.insert(nam.clone(), adt);
    }
    Ok(())
  }

  fn add_rule(&mut self, name: Name, rule: Rule, builtin: bool) {
    if let Some(def) = self.defs.get_mut(&name) {
      def.rules.push(rule);
    } else {
      self.defs.insert(name.clone(), Definition { name, rules: vec![rule], builtin });
    }
  }
}

impl<'a> ParserCommons<'a> for TermParser<'a> {}

pub trait ParserCommons<'a>: Parser<'a> {
  fn labelled<T>(
    &mut self,
    parser: impl Fn(&mut Self) -> Result<T, String>,
    label: &str,
  ) -> Result<T, String> {
    match parser(self) {
      Ok(val) => Ok(val),
      Err(_) => self.expected(label),
    }
  }

  fn parse_bend_name(&mut self) -> Result<Name, String> {
    let nam = self.parse_name()?;
    Ok(Name::new(nam))
  }

  fn skip_peek_one(&mut self) -> Option<char> {
    self.skip_trivia();
    self.peek_one()
  }

  /// Checks if the next characters in the input start with the given string.
  /// Skips trivia.
  fn skip_starts_with(&mut self, text: &str) -> bool {
    self.skip_trivia();
    self.starts_with(text)
  }

  fn expected_spanned<T>(&mut self, exp: &str, ini_idx: usize, end_idx: usize) -> Result<T, String> {
    let is_eof = self.is_eof();
    let detected = DisplayFn(|f| if is_eof { write!(f, " end of input") } else { Ok(()) });
    let msg = format!("\x1b[1m- expected:\x1b[0m {}\n\x1b[1m- detected:\x1b[0m{}", exp, detected);
    self.with_ctx(Err(msg), ini_idx, end_idx)
  }

  fn with_ctx<T>(
    &mut self,
    res: Result<T, impl std::fmt::Display>,
    ini_idx: usize,
    end_idx: usize,
  ) -> Result<T, String> {
    res.map_err(|msg| {
      let ctx = highlight_error(ini_idx, end_idx, self.input());
      format!("{msg}\n{ctx}")
    })
  }

  /// Consumes text if the input starts with it. Otherwise, do nothing.
  fn try_consume(&mut self, text: &str) -> bool {
    self.skip_trivia();
    if self.starts_with(text) {
      self.consume(text).unwrap();
      true
    } else {
      false
    }
  }

  fn try_consume_keyword(&mut self, keyword: &str) -> bool {
    self.skip_trivia();

    if !self.starts_with(keyword) {
      return false;
    }
    let input = &self.input()[*self.index() + keyword.len() ..];
    let next_is_name = input.chars().next().map_or(false, is_name_char);
    if !next_is_name {
      self.consume(keyword).unwrap();
      true
    } else {
      false
    }
  }

  /// Parses a list-like structure like "[x1, x2, x3,]".
  ///
  /// `parser` is a function that parses an element of the list.
  ///
  /// If `hard_sep` the separator between elements is mandatory.
  /// Always accepts trailing separators.
  ///
  /// `min_els` determines how many elements must be parsed at minimum.
  fn list_like<T>(
    &mut self,
    parser: impl Fn(&mut Self) -> Result<T, String>,
    start: &str,
    end: &str,
    sep: &str,
    hard_sep: bool,
    min_els: usize,
  ) -> Result<Vec<T>, String> {
    self.consume(start)?;
    let mut els = vec![];
    for i in 0 .. min_els {
      els.push(parser(self)?);
      if hard_sep && !(i == min_els - 1 && self.skip_starts_with(end)) {
        self.consume(sep)?;
      } else {
        self.try_consume(sep);
      }
    }

    while !self.try_consume(end) {
      els.push(parser(self)?);
      if hard_sep && !self.skip_starts_with(end) {
        self.consume(sep)?;
      } else {
        self.try_consume(sep);
      }
    }
    Ok(els)
  }

  fn parse_oper(&mut self) -> Result<Op, String> {
    let opr = if self.try_consume("+") {
      Op::ADD
    } else if self.try_consume("-") {
      Op::SUB
    } else if self.try_consume("*") {
      Op::MUL
    } else if self.try_consume("/") {
      Op::DIV
    } else if self.try_consume("%") {
      Op::REM
    } else if self.try_consume("<") {
      Op::LTN
    } else if self.try_consume(">") {
      Op::GTN
    } else if self.try_consume("==") {
      Op::EQL
    } else if self.try_consume("!=") {
      Op::NEQ
    } else if self.try_consume("&") {
      Op::AND
    } else if self.try_consume("|") {
      Op::OR
    } else if self.try_consume("^") {
      Op::XOR
    } else {
      return self.expected("numeric operator");
    };
    Ok(opr)
  }

  fn parse_u32(&mut self) -> Result<u32, String> {
    self.skip_trivia();
    let radix = match self.peek_many(2) {
      Some("0x") => {
        self.advance_many(2);
        16
      }
      Some("0b") => {
        self.advance_many(2);
        2
      }
      _ => 10,
    };
    let num_str = self.take_while(move |c| c.is_digit(radix) || c == '_');
    let num_str = num_str.chars().filter(|c| *c != '_').collect::<String>();
    if num_str.is_empty() {
      self.expected("numeric digit")
    } else {
      u32::from_str_radix(&num_str, radix).map_err(|e| e.to_string())
    }
  }
}
