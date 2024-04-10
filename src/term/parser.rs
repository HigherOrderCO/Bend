use crate::{
  maybe_grow,
  term::{
    display::DisplayFn, Adt, Book, Definition, IntOp, MatchRule, Name, NumCtr, Op, OpType, Pattern, Rule,
    SwitchRule, Tag, Term,
  },
};
use highlight_error::highlight_error;
use interner::global::GlobalPool;
use TSPL::Parser;

pub static STRINGS: GlobalPool<String> = GlobalPool::new();

// hvml grammar description:
// <Book>       ::= (<Data> | <Rule>)*
// <Data>       ::= "data" <Name> "=" ( <Name> | "(" <Name> (<Name>)* ")" )+
// <Rule>       ::= ("(" <Name> <Pattern>* ")" | <Name> <Pattern>*) "=" <Term>
// <Pattern>    ::= "(" <Name> <Pattern>* ")" | <NameEra> | <Number> | "(" <Pattern> ("," <Pattern>)+ ")"
// <Term>       ::=
//   <Number> | <NumOp> | <Tup> | <App> | <Group> | <Nat> | <Lam> | <UnscopedLam> |
//   <Use> | <Dup> | <LetTup> | <Let> | <Match> | <Switch> | <Era> | <UnscopedVar> | <Var>
// <Lam>        ::= <Tag>? ("λ"|"@") <NameEra> <Term>
// <UnscopedLam>::= <Tag>? ("λ"|"@") "$" <Name> <Term>
// <NumOp>      ::= "(" <Operator> <Term> <Term> ")"
// <Tup>        ::= "(" <Term> ("," <Term>)+ ")"
// <App>        ::= <Tag>? "(" <Term> (<Term>)+ ")"
// <Group>      ::= "(" <Term> ")"
// <Use>        ::= "use" <Name> "=" <Term> ";"? <Term>
// <Let>        ::= "let" <NameEra> "=" <Term> ";"? <Term>
// <LetTup>     ::= "let" "(" <NameEra> ("," <NameEra>)+ ")" "=" <Term> ";"? <Term>
// <Dup>        ::= "let" <Tag>? "{" <NameEra> (","? <NameEra>)+ "}" "=" <Term> ";"? <Term>
// <List>       ::= "[" (<Term> ","?)* "]"
// <String>     ::= "\"" (escape sequence | [^"])* "\""
// <Char>       ::= "'" (escape sequence | [^']) "'"
// <Match>      ::= "match" (<Term> | <Name> "=" <Term>) ("with" <Var> (","? <Var>)*)? "{" <MatchArm>+ "}"
// <MatchArm>   ::= "|"? <Pattern> ":" <Term> ";"?
// <Switch>     ::= "match" (<Term> | <Name> "=" <Term>) ("with" <Var> (","? <Var>)*)? "{" <SwitchArm>+ "}"
// <SwitchArm>  ::= "|"? (<Num>|"_") ":" <Term> ";"?
// <Var>        ::= <Name>
// <UnscopedVar>::= "$" <Name>
// <NameEra>    ::= <Name> | "*"
// <Era>        ::= "*"
// <Tag>        ::= "#" <Name>
// <Name>       ::= [_\-./a-zA-Z0-9]+
// <Number>     ::= ([0-9]+ | "0x"[0-9a-fA-F]+ | "0b"[01]+)
// <Operator>   ::= ( "+" | "-" | "*" | "/" | "%" | "==" | "!=" | "<<" | ">>" | "<=" | ">=" | "<" | ">" | "^" )

TSPL::new_parser!(TermParser);

impl<'a> TermParser<'a> {
  // TODO: Since TSPL doesn't expose `new` we need something that creates the parser.
  pub fn new_book(input: &'a str, default_book: Book, builtin: bool) -> Result<Book, String> {
    Self::new(input).parse_book(default_book, builtin)
  }

  pub fn new_term(input: &'a str) -> Result<Term, String> {
    Self::new(input).parse_term()
  }

  /* AST parsing functions */

  fn parse_book(&mut self, default_book: Book, builtin: bool) -> Result<Book, String> {
    let mut book = default_book;
    self.skip_trivia();
    while !self.is_eof() {
      let ini_idx = *self.index();
      if self.skip_starts_with("data") {
        // adt declaration
        let (nam, adt) = self.parse_datatype(builtin)?;
        let end_idx = *self.index();
        book.add_adt(nam, adt).map_err(|e| add_ctx(&e, ini_idx, end_idx, self.input()))?;
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
    let name = self.labelled(|p| p.parse_hvml_name(), "datatype name")?;
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
    if self.skip_starts_with("(") {
      // (name field*)
      let field_parser = |p: &mut Self| p.labelled(|p| p.parse_hvml_name(), "datatype constructor field");
      let mut els = self.list_like(field_parser, "(", ")", "", false, 1)?;
      let fields = els.split_off(1);
      let name = els.into_iter().next().unwrap();
      Ok((name, fields))
    } else {
      // name
      let name = self.labelled(|p| p.parse_hvml_name(), "datatype constructor name")?;
      Ok((name, vec![]))
    }
  }

  fn parse_rule(&mut self) -> Result<(Name, Rule), String> {
    let (name, pats) = if self.try_consume("(") {
      let name = self.labelled(|p| p.parse_hvml_name(), "function name")?;
      let pats = self.list_like(|p| p.parse_rule_pattern(), "", ")", "", false, 0)?;
      (name, pats)
    } else {
      let name = self.labelled(|p| p.parse_hvml_name(), "top-level definition")?;
      let mut pats = vec![];
      while !self.skip_starts_with("=") {
        pats.push(self.parse_rule_pattern()?);
      }
      (name, pats)
    };

    self.consume("=")?;

    let body = self.parse_term()?;

    let rule = Rule { pats, body };
    Ok((name, rule))
  }

  fn parse_rule_pattern(&mut self) -> Result<Pattern, String> {
    maybe_grow(|| {
      let Some(head) = self.skip_peek_one() else { return self.expected("pattern-matching pattern") };
      let pat = match head {
        // Ctr or Tup
        '(' => {
          self.consume("(")?;
          let head_ini_idx = *self.index();
          let head = self.parse_rule_pattern()?;
          let head_end_idx = *self.index();

          if self.try_consume(",") {
            // Tup
            let mut els = self.list_like(|p| p.parse_rule_pattern(), "", ")", ",", true, 1)?;
            els.insert(0, head);
            Pattern::Tup(els)
          } else {
            // Ctr
            let Pattern::Var(Some(name)) = head else {
              return self.expected_spanned("constructor name", head_ini_idx, head_end_idx);
            };
            let els = self.list_like(|p| p.parse_rule_pattern(), "", ")", "", false, 0)?;
            Pattern::Ctr(name, els)
          }
        }
        // List
        '[' => {
          let els = self.list_like(|p| p.parse_rule_pattern(), "[", "]", ",", false, 0)?;
          Pattern::Lst(els)
        }
        // String
        '\"' => {
          let str = self.parse_quoted_string()?;
          Pattern::Str(STRINGS.get(str))
        }
        // Char
        '\'' => {
          let char = self.parse_quoted_char()?;
          Pattern::Num(char as u64)
        }
        // Number
        c if c.is_numeric() => {
          let num = self.parse_u64()?;
          Pattern::Num(num)
        }
        // Var
        _ => {
          let name = self.parse_name_or_era()?;
          Pattern::Var(name)
        }
      };
      Ok(pat)
    })
  }

  fn parse_term(&mut self) -> Result<Term, String> {
    maybe_grow(|| {
      let Some(head) = self.skip_peek_one() else { return self.expected("term") };
      let term = match head {
        // Lambda, unscoped lambda
        'λ' | '@' => self.parse_lambda(Tag::Static)?,
        // App, Tup, Num Op
        '(' => {
          self.consume("(")?;
          let starts_with_oper = self.skip_peek_one().map_or(false, |c| "+-*/%&|<>^=!".contains(c));
          if starts_with_oper {
            let opr = self.parse_oper()?;
            if self.skip_starts_with(",")
              && let Op { ty: _, op: IntOp::Mul } = opr
            {
              // jk, actually a tuple
              let mut els = vec![Term::Era];
              while self.try_consume(",") {
                els.push(self.parse_term()?);
              }
              self.consume(")")?;
              Term::Tup { els }
            } else {
              let fst = self.parse_term()?;
              let snd = self.parse_term()?;
              self.consume(")")?;
              Term::Opx { opr, fst: Box::new(fst), snd: Box::new(snd) }
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
              Term::Tup { els }
            } else {
              // App
              let els = self.list_like(|p| p.parse_term(), "", ")", "", false, 0)?;
              els.into_iter().fold(head, |fun, arg| Term::App {
                tag: Tag::Static,
                fun: Box::new(fun),
                arg: Box::new(arg),
              })
            }
          }
        }
        // List
        '[' => {
          let els = self.list_like(|p| p.parse_term(), "[", "]", ",", false, 0)?;
          Term::Lst { els }
        }
        // Sup
        '{' => {
          let els = self.list_like(|p| p.parse_term(), "{", "}", ",", false, 2)?;
          Term::Sup { tag: Tag::Auto, els }
        }
        // Unscoped var
        '$' => {
          self.consume("$")?;
          let nam = self.parse_hvml_name()?;
          Term::Lnk { nam }
        }
        // Era
        '*' => {
          self.consume("*")?;
          Term::Era
        }
        // Nat, tagged lambda, tagged sup, tagged app
        '#' => {
          let Some(head) = self.peek_many(2) else { return self.expected("tagged term or nat") };
          let head = head.chars().collect::<Vec<_>>();
          if head[1].is_numeric() {
            // Nat
            self.consume("#")?;
            let val = self.parse_u64()?;
            Term::Nat { val }
          } else {
            // Tagged term
            let tag = self.parse_tag()?;
            let Some(head) = self.skip_peek_one() else { return self.expected("tagged term") };
            match head {
              // Tagged app
              '(' => {
                let els = self.list_like(|p| p.parse_term(), "(", ")", "", false, 2)?;
                els
                  .into_iter()
                  .reduce(|fun, arg| Term::App { tag: tag.clone(), fun: Box::new(fun), arg: Box::new(arg) })
                  .unwrap()
              }
              // Tagged sup
              '{' => {
                let els = self.list_like(|p| p.parse_term(), "{", "}", ",", false, 2)?;
                Term::Sup { tag, els }
              }
              // Tagged lambda
              'λ' | '@' => self.parse_lambda(tag)?,
              _ => return self.expected("tagged term"),
            }
          }
        }
        // String
        '"' => {
          let val = self.parse_quoted_string()?;
          Term::Str { val: STRINGS.get(val) }
        }
        // Char
        '\'' => {
          let chr = self.parse_quoted_char()?;
          Term::Num { val: chr as u64 }
        }
        // Native num
        c if c.is_numeric() => {
          let val = self.parse_u64()?;
          Term::Num { val }
        }
        _ => {
          if self.try_consume("use") {
            // Use
            let nam = self.parse_hvml_name()?;
            self.consume("=")?;
            let val = self.parse_term()?;
            self.try_consume(";");
            let nxt = self.parse_term()?;
            Term::Use { nam: Some(nam), val: Box::new(val), nxt: Box::new(nxt) }
          } else if self.try_consume("let") {
            // Let, let tup, dup, tagged dup
            let Some(head) = self.skip_peek_one() else { return self.expected("let bind") };
            match head {
              // tagged dup
              '#' => {
                let tag = self.parse_tag()?;
                let bnd = self.list_like(|p| p.parse_name_or_era(), "{", "}", ",", false, 2)?;
                self.consume("=")?;
                let val = self.parse_term()?;
                self.try_consume(";");
                let nxt = self.parse_term()?;
                Term::Dup { tag, bnd, val: Box::new(val), nxt: Box::new(nxt) }
              }
              // dup
              '{' => {
                let bnd = self.list_like(|p| p.parse_name_or_era(), "{", "}", ",", false, 2)?;
                self.consume("=")?;
                let val = self.parse_term()?;
                self.try_consume(";");
                let nxt = self.parse_term()?;
                Term::Dup { tag: Tag::Auto, bnd, val: Box::new(val), nxt: Box::new(nxt) }
              }
              // Let tup
              '(' => {
                let bnd = self.list_like(|p| p.parse_name_or_era(), "(", ")", ",", true, 2)?;
                self.consume("=")?;
                let val = self.parse_term()?;
                self.try_consume(";");
                let nxt = self.parse_term()?;
                Term::Ltp { bnd, val: Box::new(val), nxt: Box::new(nxt) }
              }
              // let
              _ => {
                let nam = self.parse_name_or_era()?;
                self.consume("=")?;
                let val = self.parse_term()?;
                self.try_consume(";");
                let nxt = self.parse_term()?;
                Term::Let { nam, val: Box::new(val), nxt: Box::new(nxt) }
              }
            }
          } else if self.try_consume("match") {
            // match
            let arg_ini_idx = *self.index();
            let mut arg = self.parse_term()?;
            let arg_end_idx = *self.index();

            let (bnd, arg) = if self.skip_starts_with("=") {
              if let Term::Var { nam } = &mut arg {
                self.consume("=")?;
                let term = self.parse_term()?;
                (Some(std::mem::take(nam)), term)
              } else {
                return self.expected_spanned("var", arg_ini_idx, arg_end_idx);
              }
            } else {
              (None, arg)
            };
            let with = if self.try_consume("with") {
              let mut with = vec![self.parse_hvml_name()?];
              while !self.skip_starts_with("{") {
                self.try_consume(",");
                with.push(self.parse_hvml_name()?);
              }
              with
            } else {
              vec![]
            };
            let rules = self.list_like(|p| p.parse_match_arm(), "{", "}", ";", false, 1)?;
            if let Some(bnd) = bnd {
              Term::Let {
                nam: Some(bnd.clone()),
                val: Box::new(arg),
                nxt: Box::new(Term::Mat { arg: Box::new(Term::Var { nam: bnd }), with, rules }),
              }
            } else {
              Term::Mat { arg: Box::new(arg), with, rules }
            }
          } else if self.try_consume("switch") {
            // switch
            let arg_ini_idx = *self.index();
            let mut arg = self.parse_term()?;
            let arg_end_idx = *self.index();

            let (bnd, arg) = if self.skip_starts_with("=") {
              if let Term::Var { nam } = &mut arg {
                self.consume("=")?;
                let term = self.parse_term()?;
                (Some(std::mem::take(nam)), term)
              } else {
                return self.expected_spanned("var", arg_ini_idx, arg_end_idx);
              }
            } else {
              (None, arg)
            };
            let with = if self.try_consume("with") {
              let mut with = vec![self.parse_hvml_name()?];
              while !self.skip_starts_with("{") {
                self.try_consume(",");
                with.push(self.parse_hvml_name()?);
              }
              with
            } else {
              vec![]
            };
            // TODO: we could enforce correct switches at the parser level to get a spanned error.
            let rules = self.list_like(|p| p.parse_switch_arm(), "{", "}", ";", false, 1)?;
            if let Some(bnd) = bnd {
              Term::Let {
                nam: Some(bnd.clone()),
                val: Box::new(arg),
                nxt: Box::new(Term::Swt { arg: Box::new(Term::Var { nam: bnd }), with, rules }),
              }
            } else {
              Term::Swt { arg: Box::new(arg), with, rules }
            }
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

  fn parse_oper(&mut self) -> Result<Op, String> {
    let opr = if self.try_consume("+") {
      Op { ty: OpType::U60, op: IntOp::Add }
    } else if self.try_consume("-") {
      Op { ty: OpType::U60, op: IntOp::Sub }
    } else if self.try_consume("*") {
      Op { ty: OpType::U60, op: IntOp::Mul }
    } else if self.try_consume("/") {
      Op { ty: OpType::U60, op: IntOp::Div }
    } else if self.try_consume("%") {
      Op { ty: OpType::U60, op: IntOp::Rem }
    } else if self.try_consume("<<") {
      Op { ty: OpType::U60, op: IntOp::Shl }
    } else if self.try_consume(">>") {
      Op { ty: OpType::U60, op: IntOp::Shr }
    } else if self.try_consume("<=") {
      Op { ty: OpType::U60, op: IntOp::Le }
    } else if self.try_consume(">=") {
      Op { ty: OpType::U60, op: IntOp::Ge }
    } else if self.try_consume("<") {
      Op { ty: OpType::U60, op: IntOp::Lt }
    } else if self.try_consume(">") {
      Op { ty: OpType::U60, op: IntOp::Gt }
    } else if self.try_consume("==") {
      Op { ty: OpType::U60, op: IntOp::Eq }
    } else if self.try_consume("!=") {
      Op { ty: OpType::U60, op: IntOp::Ne }
    } else if self.try_consume("&") {
      Op { ty: OpType::U60, op: IntOp::And }
    } else if self.try_consume("|") {
      Op { ty: OpType::U60, op: IntOp::Or }
    } else if self.try_consume("^") {
      Op { ty: OpType::U60, op: IntOp::Xor }
    } else {
      return self.expected("numeric operator");
    };
    Ok(opr)
  }

  fn parse_lambda(&mut self, tag: Tag) -> Result<Term, String> {
    self.advance_one().unwrap();
    let term = if self.try_consume("$") {
      // unscoped lambda
      let nam = self.parse_hvml_name()?;
      let bod = self.parse_term()?;
      Term::Chn { tag, nam: Some(nam), bod: Box::new(bod) }
    } else {
      // normal lambda
      let nam = self.parse_name_or_era()?;
      let bod = self.parse_term()?;
      Term::Lam { tag, nam, bod: Box::new(bod) }
    };
    Ok(term)
  }

  fn parse_hvml_name(&mut self) -> Result<Name, String> {
    let nam = self.parse_name()?;
    Ok(Name::new(nam))
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

  fn parse_tag(&mut self) -> Result<Tag, String> {
    self.consume("#")?;
    let nam = self.labelled(|p| p.parse_hvml_name(), "tag name")?;
    Ok(Tag::Named(nam))
  }

  fn parse_match_arm(&mut self) -> Result<MatchRule, String> {
    self.try_consume("|");
    let nam = self.parse_name_or_era()?;
    self.consume(":")?;
    let bod = self.parse_term()?;
    Ok((nam, vec![], bod))
  }

  fn parse_switch_arm(&mut self) -> Result<SwitchRule, String> {
    self.try_consume("|");
    let Some(head) = self.skip_peek_one() else { return self.expected("switch pattern") };
    let ctr = match head {
      '_' => {
        self.consume("_")?;
        NumCtr::Succ(None)
      }
      c if c.is_numeric() => {
        let val = self.parse_u64()?;
        NumCtr::Num(val)
      }
      _ => return self.expected("switch pattern"),
    };
    self.consume(":")?;
    let bod = self.parse_term()?;
    Ok((ctr, bod))
  }

  /* Utils */

  /// Checks if the next characters in the input start with the given string.
  /// Skips trivia.
  fn skip_starts_with(&mut self, text: &str) -> bool {
    self.skip_trivia();
    self.starts_with(text)
  }

  fn skip_peek_one(&mut self) -> Option<char> {
    self.skip_trivia();
    self.peek_one()
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

  /* Overrides */

  /// Generates an error message for parsing failures, including the highlighted context.
  ///
  /// Override to have our own error message.
  fn expected<T>(&mut self, exp: &str) -> Result<T, String> {
    let ini_idx = *self.index();
    let end_idx = *self.index() + 1;
    self.expected_spanned(exp, ini_idx, end_idx)
  }

  fn expected_spanned<T>(&mut self, exp: &str, ini_idx: usize, end_idx: usize) -> Result<T, String> {
    let ctx = highlight_error(ini_idx, end_idx, self.input());
    let is_eof = self.is_eof();
    let detected = DisplayFn(|f| if is_eof { write!(f, " end of input") } else { write!(f, "\n{ctx}") });
    Err(format!("\x1b[1m- expected:\x1b[0m {}\n\x1b[1m- detected:\x1b[0m{}", exp, detected))
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

  /// Parses a name from the input, supporting alphanumeric characters, underscores, periods, and hyphens.
  ///
  /// Override to call our own `expected`.
  fn parse_name(&mut self) -> Result<String, String> {
    self.skip_trivia();
    let name = self.take_while(|c| c.is_ascii_alphanumeric() || c == '_' || c == '.' || c == '-' || c == '/');
    if name.is_empty() { self.expected("name") } else { Ok(name.to_owned()) }
  }

  // TODO: Override because the lib has a bug where it will error on '_' .
  /// Parses a u64 from the input, supporting dec, hex (0xNUM), and bin (0bNUM).
  fn parse_u64(&mut self) -> Result<u64, String> {
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
      u64::from_str_radix(&num_str, radix).map_err(|e| e.to_string())
    }
  }

  // TODO: Override to accept more escape sequences
  /// Parses a single unicode character, supporting scape sequences.
  fn parse_char(&mut self) -> Result<char, String> {
    match self.advance_one() {
      Some('\\') => match self.advance_one() {
        Some('u' | 'U') => {
          self.consume("{")?;
          let codepoint_str = self.take_while(|c| c.is_ascii_hexdigit());
          self.consume("}")?;
          u32::from_str_radix(codepoint_str, 16)
            .ok()
            .and_then(std::char::from_u32)
            .ok_or_else(|| self.expected::<char>("unicode-codepoint").unwrap_err())
        }
        Some('n') => Ok('\n'),
        Some('r') => Ok('\r'),
        Some('t') => Ok('\t'),
        Some('\'') => Ok('\''),
        Some('\"') => Ok('\"'),
        Some('\\') => Ok('\\'),
        Some('0') => Ok('\0'),
        Some(chr) => self.expected(&format!("\\{}", chr)),
        None => self.expected("escaped-char"),
      },
      Some(other) => Ok(other),
      None => self.expected("char"),
    }
  }

  // TODO: Override to accept more escape sequences
  /// Parses a quoted character, like 'x'.
  fn parse_quoted_char(&mut self) -> Result<char, String> {
    self.skip_trivia();
    self.consume("'")?;
    let chr = self.parse_char()?;
    self.consume("'")?;
    Ok(chr)
  }

  // TODO: Override to accept more escape sequences
  /// Parses a quoted string, like "foobar".
  fn parse_quoted_string(&mut self) -> Result<String, String> {
    self.skip_trivia();
    self.consume("\"")?;
    let mut result = String::new();
    while let Some(chr) = self.peek_one() {
      if chr == '"' {
        break;
      } else {
        result.push(self.parse_char()?);
      }
    }
    self.consume("\"")?;
    Ok(result)
  }

  // TODO: override to avoid looping on ending in comment without \n
  /// Consumes the next character in the text.
  fn skip_trivia(&mut self) {
    while let Some(c) = self.peek_one() {
      if c.is_ascii_whitespace() {
        self.advance_one();
        continue;
      }
      if c == '/' && self.input().get(*self.index() ..).unwrap_or_default().starts_with("//") {
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

fn add_ctx(msg: &str, ini_idx: usize, end_idx: usize, file: &str) -> String {
  let ctx = highlight_error(ini_idx, end_idx, file);
  format!("{msg}\n{ctx}")
}
