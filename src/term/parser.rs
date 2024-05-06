use crate::{
  maybe_grow,
  term::{
    display::DisplayFn, Adt, Book, Definition, FanKind, MatchRule, Name, Op, Pattern, Rule, Tag, Term,
    STRINGS,
  },
};
use highlight_error::highlight_error;
use TSPL::Parser;

use super::NumType;

// hvml grammar description:
// <Book>       ::= (<Data> | <Rule>)*
// <Data>       ::= "data" <Name> "=" ( <Name> | "(" <Name> (<Name>)* ")" )+
// <Rule>       ::= ("(" <Name> <Pattern>* ")" | <Name> <Pattern>*) "=" <Term>
// <Pattern>    ::= "(" <Name> <Pattern>* ")" | <NameEra> | <Number> | "(" <Pattern> ("," <Pattern>)+ ")"
// <Term>       ::=
//   <Number> | <NumOp> | <Tup> | <App> | <Group> | <Nat> | <Lam> | <UnscopedLam> |
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
        book.add_adt(nam, adt).map_err(|e| add_ctx_to_msg(&e, ini_idx, end_idx, self.input()))?;
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

  fn parse_datatype_ctr(&mut self) -> Result<(Name, Vec<Name>), String> {
    if self.try_consume("(") {
      // (name field*)
      let name = self.parse_top_level_name()?;
      let field_parser = |p: &mut Self| p.labelled(|p| p.parse_hvml_name(), "datatype constructor field");
      let fields = self.list_like(field_parser, "", ")", "", false, 0)?;
      Ok((name, fields))
    } else {
      // name
      let name = self.labelled(|p| p.parse_top_level_name(), "datatype constructor name")?;
      Ok((name, vec![]))
    }
  }

  fn parse_rule(&mut self) -> Result<(Name, Rule), String> {
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
      let Some(head) = self.skip_peek_one() else { return self.expected("pattern-matching pattern") };
      let pat = match head {
        // Ctr or Tup
        '(' => {
          self.consume("(")?;
          let head_ini_idx = *self.index();
          let head = self.parse_pattern(simple)?;
          let head_end_idx = *self.index();

          if simple || self.skip_starts_with(",") {
            self.consume(",")?;
            // Tup
            let mut els = self.list_like(|p| p.parse_pattern(simple), "", ")", ",", true, 1)?;
            els.insert(0, head);
            Pattern::Fan(FanKind::Tup, tag.unwrap_or(Tag::Static), els)
          } else {
            unexpected_tag(self)?;
            // Ctr
            let Pattern::Var(Some(name)) = head else {
              return self.expected_spanned("constructor name", head_ini_idx, head_end_idx);
            };
            let els = self.list_like(|p| p.parse_pattern(simple), "", ")", "", false, 0)?;
            Pattern::Ctr(name, els)
          }
        }
        // Dup
        '{' => {
          let els = self.list_like(|p| p.parse_pattern(simple), "{", "}", "", false, 0)?;
          Pattern::Fan(FanKind::Dup, tag.unwrap_or(Tag::Auto), els)
        }
        // List
        '[' if !simple => {
          unexpected_tag(self)?;
          let els = self.list_like(|p| p.parse_pattern(simple), "[", "]", ",", false, 0)?;
          Pattern::Lst(els)
        }
        // String
        '\"' if !simple => {
          unexpected_tag(self)?;
          let str = self.parse_quoted_string()?;
          Pattern::Str(STRINGS.get(str))
        }
        // Char
        '\'' => {
          unexpected_tag(self)?;
          let char = self.parse_quoted_char()?;
          Pattern::Num(char as u32)
        }
        // Number
        c if c.is_ascii_digit() => {
          unexpected_tag(self)?;
          let num = self.parse_u64()?;
          Pattern::Num(num as u32)
        }
        // Channel
        '$' => {
          unexpected_tag(self)?;
          self.advance_one();
          let name = self.parse_hvml_name()?;
          Pattern::Chn(name)
        }
        // Var
        _ => {
          unexpected_tag(self)?;
          let name = self.parse_name_or_era()?;
          Pattern::Var(name)
        }
      };
      Ok(pat)
    })
  }

  pub fn parse_term(&mut self) -> Result<Term, String> {
    maybe_grow(|| {
      let (tag, unexpected_tag) = self.parse_tag()?;
      let Some(head) = self.skip_peek_one() else { return self.expected("term") };
      let term = match head {
        // Lambda, unscoped lambda
        'λ' | '@' => {
          let tag = tag.unwrap_or(Tag::Static);
          self.advance_one().unwrap();
          let pat = self.parse_pattern(true)?;
          let bod = self.parse_term()?;
          Term::Lam { tag, pat: Box::new(pat), bod: Box::new(bod) }
        }
        // App, Tup, Num Op
        '(' => {
          self.consume("(")?;
          let starts_with_oper = self.skip_peek_one().map_or(false, |c| "+-*/%&|<>^=!".contains(c));
          if starts_with_oper {
            let opr = self.parse_oper()?;
            if self.skip_starts_with(",") && opr == Op::MUL {
              // jk, actually a tuple
              let mut els = vec![Term::Era];
              while self.try_consume(",") {
                els.push(self.parse_term()?);
              }
              self.consume(")")?;
              Term::Fan { fan: FanKind::Tup, tag: tag.unwrap_or(Tag::Static), els }
            } else {
              unexpected_tag(self)?;
              let fst = self.parse_term()?;
              let snd = self.parse_term()?;
              self.consume(")")?;
              Term::Opr { opr, fst: Box::new(fst), snd: Box::new(snd) }
            }
          } else {
            // Tup or App
            let head = self.parse_term()?;
            if self.skip_starts_with(",") {
              // Tup
              let mut els = vec![head];
              while self.try_consume(",") {
                els.push(self.parse_term()?);
              }
              self.consume(")")?;
              Term::Fan { fan: FanKind::Tup, tag: tag.unwrap_or(Tag::Static), els }
            } else {
              // App
              let els = self.list_like(|p| p.parse_term(), "", ")", "", false, 0)?;
              els.into_iter().fold(head, |fun, arg| Term::App {
                tag: tag.clone().unwrap_or(Tag::Static),
                fun: Box::new(fun),
                arg: Box::new(arg),
              })
            }
          }
        }
        // List
        '[' => {
          unexpected_tag(self)?;
          let els = self.list_like(|p| p.parse_term(), "[", "]", ",", false, 0)?;
          Term::Lst { els }
        }
        // Sup
        '{' => {
          let els = self.list_like(|p| p.parse_term(), "{", "}", ",", false, 2)?;
          Term::Fan { fan: FanKind::Dup, tag: tag.unwrap_or(Tag::Auto), els }
        }
        // Unscoped var
        '$' => {
          unexpected_tag(self)?;
          self.consume("$")?;
          let nam = self.parse_hvml_name()?;
          Term::Lnk { nam }
        }
        // Era
        '*' => {
          unexpected_tag(self)?;
          self.consume("*")?;
          Term::Era
        }
        // Nat, tagged lambda, tagged sup, tagged app
        '#' => {
          unexpected_tag(self)?;
          self.consume("#")?;
          let val = self.parse_u64()?;
          Term::Nat { val }
        }
        // String
        '"' => {
          unexpected_tag(self)?;
          let val = self.parse_quoted_string()?;
          Term::Str { val: STRINGS.get(val) }
        }
        // Char
        '\'' => {
          unexpected_tag(self)?;
          let chr = self.parse_quoted_char()?;
          Term::Num { typ: NumType::U24, val: chr as u32 }
        }
        // Native num
        c if c.is_ascii_digit() => {
          unexpected_tag(self)?;
          let val = self.parse_u64()?;
          Term::Num { typ: NumType::U24, val: val as u32 }
        }
        _ => {
          unexpected_tag(self)?;
          if self.try_consume_keyword("use") {
            // Use
            let nam = self.parse_hvml_name()?;
            self.consume("=")?;
            let val = self.parse_term()?;
            self.try_consume(";");
            let nxt = self.parse_term()?;
            Term::Use { nam: Some(nam), val: Box::new(val), nxt: Box::new(nxt) }
          } else if self.try_consume_keyword("let") {
            let pat = self.parse_pattern(true)?;
            self.consume("=")?;
            let val = self.parse_term()?;
            self.try_consume(";");
            let nxt = self.parse_term()?;
            Term::Let { pat: Box::new(pat), val: Box::new(val), nxt: Box::new(nxt) }
          } else if self.try_consume_keyword("match") {
            // match
            let (bnd, arg, with) = self.parse_match_arg()?;
            let rules = self.list_like(|p| p.parse_match_arm(), "{", "}", ";", false, 1)?;
            Term::Mat { arg: Box::new(arg), bnd: Some(bnd), with, arms: rules }
          } else if self.try_consume_keyword("switch") {
            // switch
            self.parse_switch()?
          } else if self.try_consume_keyword("do") {
            let fun = self.parse_name()?;
            self.consume("{")?;
            let ask = self.parse_ask(Name::new(fun))?;
            self.consume("}")?;
            ask
          } else {
            // var
            let nam = self.labelled(|p| p.parse_hvml_name(), "term")?;
            Term::Var { nam }
          }
        }
      };
      Ok(term)
    })
  }

  fn parse_ask(&mut self, fun: Name) -> Result<Term, String> {
    maybe_grow(|| {
      if self.try_consume_keyword("ask") {
        let ask = self.parse_pattern(true)?;
        self.consume("=")?;
        let val = self.parse_term()?;
        self.try_consume(";");
        let nxt = self.parse_ask(fun.clone())?;
        Ok(Term::Bnd { fun, ask: Box::new(ask), val: Box::new(val), nxt: Box::new(nxt) })
      } else {
        self.parse_term()
      }
    })
  }

  fn parse_top_level_name(&mut self) -> Result<Name, String> {
    let ini_idx = *self.index();
    let nam = self.parse_hvml_name()?;
    let end_idx = *self.index();
    if nam.contains("__") {
      let ctx = highlight_error(ini_idx, end_idx, self.input());
      Err(format!("Top-level names are not allowed to contain \"__\".\n{ctx}"))
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
          let nam = p.parse_hvml_name()?;
          Ok(Some(nam))
        }
      },
      "name or '*'",
    )
  }

  /// Parses a tag where it may or may not be valid.
  ///
  /// If it is not valid, the returned callback can be used to issue an error.
  fn parse_tag(&mut self) -> Result<(Option<Tag>, impl FnOnce(&Self) -> Result<(), String>), String> {
    let index = self.index;
    let tag = if self.skip_peek_one() == Some('#')
      && !self.peek_many(2).is_some_and(|x| x.chars().nth(1).unwrap().is_ascii_digit())
    {
      let ctx = highlight_error(index, index + 1, self.input);
      return Err(format!("Tagged terms not supported for hvm32.\n{ctx}"));
      /* self.advance_one();
      let nam = self.labelled(|p| p.parse_hvml_name(), "tag name")?;
      Some(Tag::Named(nam)) */
    } else {
      None
    };
    let end_index = self.index;
    let has_tag = tag.is_some();
    Ok((tag, move |slf: &Self| {
      if has_tag {
        let ctx = highlight_error(index, end_index, slf.input);
        Err(format!("\x1b[1m- unexpected tag:\x1b[0m{}", ctx))
      } else {
        Ok(())
      }
    }))
  }

  fn parse_match_arg(&mut self) -> Result<(Name, Term, Vec<Name>), String> {
    let bnd = self.parse_hvml_name()?;
    let arg = if self.try_consume("=") { self.parse_term()? } else { Term::Var { nam: bnd.clone() } };
    let with = if self.try_consume_keyword("with") {
      let mut with = vec![self.parse_hvml_name()?];
      while !self.skip_starts_with("{") {
        self.try_consume(",");
        with.push(self.parse_hvml_name()?);
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
    let (bnd, arg, with) = self.parse_match_arg()?;
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
          let val = self.parse_u64()?;
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
    let pred = Some(Name::new(format!("{}-{}", bnd, arms.len() - 1)));
    self.consume("}")?;
    Ok(Term::Swt { arg: Box::new(arg), bnd: Some(bnd), with, pred, arms })
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

fn add_ctx_to_msg(msg: &str, ini_idx: usize, end_idx: usize, file: &str) -> String {
  let ctx = highlight_error(ini_idx, end_idx, file);
  format!("{msg}\n{ctx}")
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

  fn parse_hvml_name(&mut self) -> Result<Name, String> {
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
    let ctx = highlight_error(ini_idx, end_idx, self.input());
    let is_eof = self.is_eof();
    let detected = DisplayFn(|f| if is_eof { write!(f, " end of input") } else { write!(f, "\n{ctx}") });
    Err(format!("\x1b[1m- expected:\x1b[0m {}\n\x1b[1m- detected:\x1b[0m{}", exp, detected))
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
}
