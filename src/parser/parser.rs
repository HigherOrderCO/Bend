use crate::{
  ast::{Definition, DefinitionBook, Name, NumOper, Rule, Term},
  parser::lexer::Token,
};
use chumsky::{
  extra,
  input::{Emitter, Stream, ValueInput},
  prelude::{Input, Rich},
  primitive::{choice, just},
  recursive::recursive,
  select,
  span::SimpleSpan,
  IterParser, Parser,
};
use logos::Logos;

pub fn parse_definition_book(code: &str) -> Result<DefinitionBook, Vec<Rich<Token>>> {
  let token_iter = Token::lexer(code).spanned().map(|(token, span)| match token {
    Ok(t) => (t, SimpleSpan::from(span)),
    Err(e) => (Token::Error(e), SimpleSpan::from(span)),
  });
  let token_stream = Stream::from_iter(token_iter).spanned(SimpleSpan::from(code.len() .. code.len()));
  book_parser().parse(token_stream).into_result()
}

// TODO: Pattern matching on rules
// TODO: Other types of numbers
/// <Name>   ::= <name_token> // [_a-zA-Z][_a-zA-Z0-9]*
/// <Number> ::= <number_token> // [0-9]+
/// <Var>    ::= <Name>
/// <Nested> ::= "(" <newline_token>* <Term> <newline_token>* ")"
/// <Item>   ::= <Var> | <Number> | <Nested>
/// <App>    ::= <Item> <Item>+
/// <Lam>    ::= ("λ"|"\") <Name> <Term>
/// <Dup>    ::= "dup" <Name> <Name> "=" <Term> ";" <Term>
/// <Let>    ::= "let" <Name> "=" <Term> ";" <Term>
/// <NumOp>  ::= <numop_token> <Item> <Item>
/// <Term>   ::= <Lam> | <App> | <Dup> | <Let> | <NumOp> | <Item>
/// <Rule>   ::= "(" <Name> ")" "=" <newline_token>* <Term> <newline_token>*
/// <Def>    ::= (<Rule> <NewLine>)+
/// <Book>   ::= <Def>+

fn rule_parser<'a, I>() -> impl Parser<'a, I, Rule, extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  let new_line = just(Token::NewLine).repeated();
  let name = select!(Token::Name(name) => Name(name.to_string()));
  let number = select!(Token::Number(num) => Term::Num{val: num});

  let num_oper = select! {
    Token::Add => NumOper::Add,
    Token::Sub => NumOper::Sub,
    Token::Mul => NumOper::Mul,
    Token::Div => NumOper::Div,
    Token::Mod => NumOper::Mod,
    Token::And => NumOper::And,
    Token::Or => NumOper::Or,
    Token::Xor => NumOper::Xor,
    Token::Shl => NumOper::Shl,
    Token::Shr => NumOper::Shr,
    Token::Lte => NumOper::Lte,
    Token::Ltn => NumOper::Ltn,
    Token::Gte => NumOper::Gte,
    Token::Gtn => NumOper::Gtn,
    Token::EqualsEquals => NumOper::Eql,
    Token::NotEquals => NumOper::Neq,
  };

  let var = name.map(|name| Term::Var { nam: name });

  let term = recursive(|term| {
    let nested = term
      .clone()
      .delimited_by(just(Token::LParen).then(new_line.clone()), just(Token::RParen).then(new_line.clone()));

    let item = choice((var, number, nested));

    let app = item
      .clone()
      .foldl(item.clone().repeated(), |fun, arg| Term::App { fun: Box::new(fun), arg: Box::new(arg) });

    let lam = just(Token::Lambda)
      .ignore_then(name)
      .then(term.clone())
      .map(|(name, body)| Term::Lam { nam: name, bod: Box::new(body) });

    let dup = just(Token::Dup)
      .ignore_then(name)
      .then(name)
      .then_ignore(just(Token::Equals))
      .then(term.clone())
      .then_ignore(just(Token::Semicolon))
      .then(term.clone())
      .map(|(((fst, snd), val), next)| Term::Dup { fst, snd, val: Box::new(val), nxt: Box::new(next) });

    let let_ = just(Token::Let)
      .ignore_then(name)
      .then_ignore(just(Token::Equals))
      .then(term.clone())
      .then_ignore(just(Token::Semicolon))
      .then(term.clone())
      .map(|((name, body), next)| Term::App {
        fun: Box::new(Term::Lam { nam: name, bod: next.into() }),
        arg: Box::new(body),
      });

    let num_op = num_oper.then(item.clone()).then(item.clone()).map(|((op, fst), snd)| Term::NumOp {
      op,
      fst: Box::new(fst),
      snd: Box::new(snd),
    });

    choice((lam, app, dup, let_, num_op, item))
  });

  just(Token::LParen)
    .ignore_then(name)
    .then_ignore(just(Token::RParen))
    .then_ignore(just(Token::Equals))
    .then(term.delimited_by(new_line.clone(), new_line.clone()))
    .map(|(name, body)| Rule { name, pats: vec![], body })
}

fn book_parser<'a, I>() -> impl Parser<'a, I, DefinitionBook, extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  fn rules_to_book(
    rules: Vec<(Rule, SimpleSpan)>,
    _span: SimpleSpan,
    emitter: &mut Emitter<Rich<Token>>,
  ) -> DefinitionBook {
    let mut book = DefinitionBook::new();
    let mut crnt_def: Option<(Definition, SimpleSpan)> = None;
    // Check for repeated defs (could be rules out of order or actually repeated names)
    for (rule, rule_span) in rules {
      if let Some((crnt_def, def_span)) = crnt_def.as_mut() {
        if rule.name == crnt_def.name {
          emitter
            .emit(Rich::custom(*def_span, format!("Definition with multiple rules '{}'", *crnt_def.name)));
          // TODO: Enable definitions with multiple rules when implementing pattern matching
          // crnt_def.rules.push(rule);
          // def_span.end = rule_span.end;
        } else {
          let def = std::mem::replace(crnt_def, Definition { name: rule.name, rules: vec![] });
          add_to_def_book(&mut book, def, *def_span, emitter);
        }
      } else {
        crnt_def = Some((Definition { name: rule.name.clone(), rules: vec![rule] }, rule_span));
      }
    }
    if let Some((def, span)) = crnt_def {
      add_to_def_book(&mut book, def, span, emitter);
    }
    book
  }

  fn add_to_def_book(
    book: &mut DefinitionBook,
    def: Definition,
    span: SimpleSpan,
    emitter: &mut Emitter<Rich<Token>>,
  ) {
    if book.defs.contains_key(&def.name) {
      emitter.emit(Rich::custom(span, format!("Repeated definition '{}'", *def.name)));
    } else {
      book.defs.insert(def.name.clone(), def);
    }
  }

  let parsed_rules =
    rule_parser().map_with_span(|rule, span| (rule, span)).repeated().collect::<Vec<(Rule, SimpleSpan)>>();

  parsed_rules.validate(move |rules, span, emitter| rules_to_book(rules, span, emitter))
}
