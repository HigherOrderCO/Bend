use super::lexer::LexingError;
use crate::{
  ast::{DefId, Definition, DefinitionBook, Name, NumOper, Rule, Term},
  parser::lexer::Token,
};
use chumsky::{
  extra,
  input::{Emitter, SpannedInput, Stream, ValueInput},
  prelude::{Input, Rich},
  primitive::{choice, just},
  recursive::recursive,
  select,
  span::SimpleSpan,
  IterParser, Parser,
};
use logos::{Logos, SpannedIter};
use std::{collections::hash_map, iter::Map, ops::Range};

// TODO: Pattern matching on rules
// TODO: Other types of numbers
/// <Book>   ::= <Def>* // Sequential rules grouped by name
/// <Def>    ::= \n* <Rule> (\n+ <Rule>)* \n*
/// <Rule>   ::= ("(" <Name> ")" | <Name>) \n* "=" \n* (<InlineNumOp> | <InlineApp>)
/// <InlineNumOp> ::= <numop_token> <Term> <Term>
/// <InlineApp>   ::= <Term>+
/// <Term>   ::= <Var> | <Number> | <Lam> | <Dup> | <Let> | <NumOp> | <App>
/// <Lam>    ::= ("λ"|"@") \n* <Name> \n* <Term>
/// <Dup>    ::= "dup" \n* <Name> \n* <Name> \n* "=" \n* <Term> (\n+ | \n* ";") \n* <Term>
/// <Let>    ::= "let" \n* <Name> \n* "=" \n* <Term> (\n+ | \n* ";") \n* <Term>
/// <NumOp>  ::= "(" \n* <numop_token> \n* <Term> \n* <Term> \n* ")"
/// <App>    ::= "(" \n* <Term> (\n* <Term>)* \n* ")"
/// <Var>    ::= <Name>
/// <Name>   ::= <name_token> // [_a-zA-Z][_a-zA-Z0-9]{0..7}
/// <Number> ::= <number_token> // [0-9]+
pub fn parse_definition_book(code: &str) -> Result<DefinitionBook, Vec<Rich<Token>>> {
  book_parser().parse(token_stream(code)).into_result()
}

pub fn parse_term(code: &str) -> Result<Term, Vec<Rich<Token>>> {
  let inline_app =
    term().foldl(term().repeated(), |fun, arg| Term::App { fun: Box::new(fun), arg: Box::new(arg) });
  let inline_num_oper = num_oper().then(term()).then(term()).map(|((op, fst), snd)| Term::NumOp {
    op,
    fst: Box::new(fst),
    snd: Box::new(snd),
  });
  let standalone_term = choice((inline_app, inline_num_oper))
    .delimited_by(just(Token::NewLine).repeated(), just(Token::NewLine).repeated());

  // TODO: Make a function that calls a parser. I couldn't figure out how to type it correctly.
  standalone_term.parse(token_stream(code)).into_result()
}

fn token_stream(
  code: &str,
) -> SpannedInput<
  Token,
  SimpleSpan,
  Stream<
    Map<SpannedIter<Token>, impl FnMut((Result<Token, LexingError>, Range<usize>)) -> (Token, SimpleSpan)>,
  >,
> {
  // TODO: Maybe change to just using chumsky.
  // The integration is not so smooth and we need to figure out
  // errors, spans and other things that are not so obvious.
  let token_iter = Token::lexer(code).spanned().map(|(token, span)| match token {
    Ok(t) => (t, SimpleSpan::from(span)),
    Err(e) => (Token::Error(e), SimpleSpan::from(span)),
  });
  Stream::from_iter(token_iter).spanned(SimpleSpan::from(code.len() .. code.len()))
}

// Parsers
const MAX_NAME_LEN: usize = ((u64::BITS - u16::BITS) / 64_u32.ilog2()) as usize;

fn name<'a, I>() -> impl Parser<'a, I, Name, extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  select!(Token::Name(name) => Name(name)).try_map(|name, span| {
    if name.len() > MAX_NAME_LEN {
      // TODO: Implement some kind of name mapping for definitions so that we can fit any def size.
      // e.g. sequential mapping, mangling, hashing, etc
      Err(Rich::custom(span, format!("'{}' exceed maximum name length of {}", *name, MAX_NAME_LEN)))
    } else {
      Ok(name)
    }
  })
}

fn num_oper<'a, I>() -> impl Parser<'a, I, NumOper, extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  select! {
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
  }
}

fn term<'a, I>() -> impl Parser<'a, I, Term, extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  let new_line = || just(Token::NewLine).repeated();
  let number = select!(Token::Number(num) => Term::Num{val: num});
  let var = name().map(|name| Term::Var { nam: name }).boxed();

  recursive(|term| {
    // λx body
    let lam = just(Token::Lambda)
      .ignore_then(new_line())
      .ignore_then(name())
      .then_ignore(new_line())
      .then(term.clone())
      .map(|(name, body)| Term::Lam { nam: name, bod: Box::new(body) })
      .boxed();

    // dup x1 x2 = body; next
    let dup = just(Token::Dup)
      .ignore_then(new_line())
      .ignore_then(name())
      .then_ignore(new_line())
      .then(name())
      .then_ignore(new_line())
      .then_ignore(just(Token::Equals))
      .then_ignore(new_line())
      .then(term.clone())
      .then_ignore(just(Token::Semicolon))
      .then_ignore(new_line())
      .then(term.clone())
      .map(|(((fst, snd), val), next)| Term::Dup { fst, snd, val: Box::new(val), nxt: Box::new(next) })
      .boxed();

    // let x = body; next
    let let_ = just(Token::Let)
      .ignore_then(new_line())
      .ignore_then(name())
      .then_ignore(new_line())
      .then_ignore(just(Token::Equals))
      .then_ignore(new_line())
      .then(term.clone())
      .then_ignore(new_line())
      .then_ignore(just(Token::Semicolon))
      .then_ignore(new_line())
      .then(term.clone())
      .map(|((name, body), next)| Term::App {
        fun: Box::new(Term::Lam { nam: name, bod: next.into() }),
        arg: Box::new(body),
      })
      .boxed();

    // (f arg1 arg2 ...)
    let app = term
      .clone()
      .foldl(new_line().ignore_then(term.clone()).repeated(), |fun, arg| Term::App {
        fun: Box::new(fun),
        arg: Box::new(arg),
      })
      .delimited_by(new_line(), new_line())
      .delimited_by(just(Token::LParen), just(Token::RParen))
      .boxed();

    let num_op = num_oper()
      .then_ignore(new_line())
      .then(term.clone())
      .then_ignore(new_line())
      .then(term.clone())
      .delimited_by(new_line(), new_line())
      .delimited_by(just(Token::LParen), just(Token::RParen))
      .map(|((op, fst), snd)| Term::NumOp { op, fst: Box::new(fst), snd: Box::new(snd) })
      .boxed();

    choice((var, number, lam, dup, let_, num_op, app))
  })
}

fn rule<'a, I>() -> impl Parser<'a, I, Rule, extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  let inline_app =
    term().foldl(term().repeated(), |fun, arg| Term::App { fun: Box::new(fun), arg: Box::new(arg) });
  let inline_num_oper = num_oper().then(term()).then(term()).map(|((op, fst), snd)| Term::NumOp {
    op,
    fst: Box::new(fst),
    snd: Box::new(snd),
  });

  choice((name(), name().delimited_by(just(Token::LParen), just(Token::RParen))))
    .then_ignore(just(Token::NewLine).repeated())
    .then_ignore(just(Token::Equals))
    .then_ignore(just(Token::NewLine).repeated())
    .then(choice((inline_num_oper, inline_app)))
    .map(|(name, body)| Rule { def_id: DefId::from(&name), pats: vec![], body })
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

    // Check for repeated defs (could be rules out of order or actually repeated names)
    for def_rules in rules.group_by(|(rule1, _), (rule2, _)| rule1.def_id == rule2.def_id) {
      let name = Name::from(def_rules[0].0.def_id);
      if def_rules.len() > 1 {
        // TODO: Enable definitions with multiple rules when implementing pattern matching
        let def_span = SimpleSpan::new(def_rules.first().unwrap().1.start, def_rules.last().unwrap().1.end);
        emitter.emit(Rich::custom(def_span, format!("Definition with multiple rules '{name}'",)));
      } else {
        let (rule, span) = &def_rules[0];
        let def = Definition { name, rules: vec![rule.clone()] };
        let def_id = DefId::from(&def.name);
        if let hash_map::Entry::Vacant(e) = book.defs.entry(def_id) {
          e.insert(def);
        } else {
          emitter.emit(Rich::custom(*span, format!("Repeated definition '{}'", *def.name)));
        }
      }
    }
    book
  }

  let new_line = just(Token::NewLine).repeated();

  let parsed_rules = rule()
    .map_with_span(|rule, span| (rule, span))
    .separated_by(new_line.at_least(1))
    .allow_leading()
    .allow_trailing()
    .collect::<Vec<(Rule, SimpleSpan)>>();

  parsed_rules.validate(rules_to_book)
}
