use super::lexer::LexingError;
use crate::{
  ast::{
    hvm_lang::{Pattern, SpannedTerm},
    spanned::Spanned,
    DefId, Definition, DefinitionBook, Name, Rule, Term,
  },
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
use itertools::Itertools;
use logos::{Logos, SpannedIter};
use std::{iter::Map, ops::Range};

use crate::ast::hvm_lang::Op;

// TODO: Pattern matching on rules
// TODO: Other types of numbers
/// <Book>   ::= <Def>* // Sequential rules grouped by name
/// <Def>    ::= \n* <Rule> (\n+ <Rule>)* \n*
/// <Rule>   ::= ("(" <Name> <Pattern>* ")" | <Name> <Pattern>*) \n* "=" \n* (<InlineNumOp> | <InlineApp>)
/// <Pattern> ::= "(" <Name> <Pattern>* ")" | <NameEra> | <Number>
/// <InlineNumOp> ::= <numop_token> <Term> <Term>
/// <InlineApp>   ::= <Term>+
/// <Term>   ::= <Var> | <GlobalVar> | <Number> | <Lam> | <GlobalLam> | <Dup> | <Let> | <NumOp> | <App>
/// <Lam>    ::= ("λ"|"@") \n* <NameEra> \n* <Term>
/// <GlobalLam> ::= ("λ"|"@") "$" <Name> \n* <Term>
/// <Dup>    ::= "dup" \n* <Name> \n* <Name> \n* "=" \n* <Term> (\n+ | \n* ";") \n* <Term>
/// <Let>    ::= "let" \n* <Name> \n* "=" \n* <Term> (\n+ | \n* ";") \n* <Term>
/// <NumOp>  ::= "(" \n* <numop_token> \n* <Term> \n* <Term> \n* ")"
/// <App>    ::= "(" \n* <Term> (\n* <Term>)* \n* ")"
/// <Var>    ::= <Name>
/// <GlobalVar> ::= "$" <Name>
/// <NameEra> ::= <Name> | "*"
/// <Name>   ::= <name_token> // [_a-zA-Z][_a-zA-Z0-9]{0..7}
/// <Number> ::= <number_token> // [0-9]+
pub fn parse_definition_book(code: &str) -> Result<DefinitionBook, Vec<Rich<Token>>> {
  book().parse(token_stream(code)).into_result()
}

pub fn parse_term(code: &str) -> Result<SpannedTerm, Vec<Rich<Token>>> {
  let inline_app = term().foldl(term().repeated(), |fun, arg| {
    let t = Term::App { fun: Box::new(fun), arg: Box::new(arg) };
    Spanned::new(t, fun.mix(&arg))
  });
  let inline_num_oper = num_oper().then(term()).then(term()).map(|((op, fst), snd)| {
    let span = op.mix(&snd);
    let t = Term::Opx { op, fst: Box::new(fst), snd: Box::new(snd) };
    Spanned::new(t, span)
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

fn name<'a, I>() -> impl Parser<'a, I, Name, extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  select!(Token::Name(name) => Name(name))
}

fn name_or_era<'a, I>() -> impl Parser<'a, I, Option<Name>, extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  choice((select!(Token::Asterisk => None), name().map(Some)))
}

fn num_oper<'a, I>() -> impl Parser<'a, I, Spanned<Op>, extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  select! {
    Token::Add => Op::ADD,
    Token::Sub => Op::SUB,
    Token::Asterisk => Op::MUL,
    Token::Div => Op::DIV,
    Token::Mod => Op::MOD,
    Token::EqualsEquals => Op::EQ,
    Token::NotEquals => Op::NE,
    Token::Ltn => Op::LT,
    Token::Gtn => Op::GT,
    Token::And => Op::AND,
    Token::Or => Op::OR,
    Token::Xor => Op::XOR,
    Token::Tilde => Op::NOT,
    Token::Shl => Op::LSH,
    Token::Shr => Op::RSH,
  }
  .map_with_span(|op, span: SimpleSpan| Spanned::new(op, span.into_range()))
}

fn term<'a, I>() -> impl Parser<'a, I, SpannedTerm, extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  let new_line = || just(Token::NewLine).repeated();
  let var = name()
    .map_with_span(|name, span: SimpleSpan| Spanned::new(Term::Var { nam: name }, span.into_range()))
    .boxed();
  let global_var = just(Token::Dollar)
    .ignore_then(name())
    .map_with_span(|name, span: SimpleSpan| Spanned::new(Term::Lnk { nam: name }, span.into_range()))
    .boxed();
  let term_sep = choice((just(Token::NewLine), just(Token::Semicolon)));

  let unsigned = select!(Token::Num(num) => Term::Num{val: num})
    .map_with_span(|term, span: SimpleSpan| Spanned::new(term, span.into_range()));

  recursive(|term| {
    // λx body
    let lam = just(Token::Lambda)
      .ignore_then(new_line())
      .ignore_then(name_or_era())
      .then_ignore(new_line())
      .then(term.clone())
      .map_with_span(|(name, body), span: SimpleSpan| {
        let t = Term::Lam { nam: name, bod: Box::new(body) };
        Spanned::new(t, span.into_range())
      })
      .boxed();

    // λ$x body
    let global_lam = just(Token::Lambda)
      .ignore_then(new_line())
      .ignore_then(just(Token::Dollar))
      .ignore_then(new_line())
      .ignore_then(name())
      .then_ignore(new_line())
      .then(term.clone())
      .map_with_span(|(name, body), span: SimpleSpan| {
        let t = Term::Chn { nam: name, bod: Box::new(body) };
        Spanned::new(t, span.into_range())
      })
      .boxed();

    // dup x1 x2 = body; next
    let dup = just(Token::Dup)
      .ignore_then(new_line())
      .ignore_then(name_or_era())
      .then_ignore(new_line())
      .then(name_or_era())
      .then_ignore(new_line())
      .then_ignore(just(Token::Equals))
      .then_ignore(new_line())
      .then(term.clone())
      .then_ignore(term_sep.clone())
      .then_ignore(new_line())
      .then(term.clone())
      .map_with_span(|(((fst, snd), val), next), span: SimpleSpan| {
        let t = Term::Dup { fst, snd, val: Box::new(val), nxt: Box::new(next) };
        Spanned::new(t, span.into_range())
      })
      .boxed();

    // let x = body; next
    let let_ = just(Token::Let)
      .ignore_then(new_line())
      .ignore_then(name())
      .then_ignore(new_line())
      .then_ignore(just(Token::Equals))
      .then_ignore(new_line())
      .then(term.clone())
      .then_ignore(term_sep)
      .then_ignore(new_line())
      .then(term.clone())
      .map_with_span(|((nam, val), nxt), span: SimpleSpan| {
        let t = Term::Let { nam, val: Box::new(val), nxt: Box::new(nxt) };
        Spanned::new(t, span.into_range())
      })
      .boxed();

    let if_ = just(Token::If)
      .ignore_then(new_line())
      .ignore_then(term.clone())
      .then_ignore(select!(Token::Name(nam) if nam == "then" => ()))
      .then(term.clone())
      .then_ignore(select!(Token::Name(nam) if nam == "else" => ()))
      .then(term.clone())
      .map_with_span(|((cond, then), els_), span: SimpleSpan| {
        let t = Term::If { cond: Box::new(cond), then: Box::new(then), els_: Box::new(els_) };
        Spanned::new(t, span.into_range())
      });

    // (f arg1 arg2 ...)
    let app = term
      .clone()
      .foldl(new_line().ignore_then(term.clone()).repeated(), |fun, arg| {
        let span = fun.mix(&arg);
        Spanned::new(Term::App { fun: Box::new(fun), arg: Box::new(arg) }, span)
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
      .map_with_span(|((op, fst), snd), span: SimpleSpan| {
        let t = Term::Opx { op, fst: Box::new(fst), snd: Box::new(snd) };
        Spanned::new(t, span.into_range())
      })
      .boxed();

    choice((global_var, var, unsigned, global_lam, lam, dup, let_, if_, num_op, app))
  })
}

fn pattern<'a, I>() -> impl Parser<'a, I, Pattern, extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  recursive(|pattern| {
    let ctr = name()
      .then(pattern.repeated().collect())
      .delimited_by(just(Token::LParen), just(Token::RParen))
      .map(|(name, pats)| Pattern::Ctr(name, pats))
      .boxed();
    let var = name_or_era().map(Pattern::Var).boxed();
    let num = select!(Token::Num(num) => Pattern::Num(num)).boxed();
    choice((ctr, num, var))
  })
}

fn rule<'a, I>() -> impl Parser<'a, I, (Name, Rule), extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  let inline_app = term().foldl(term().repeated(), |fun, arg| {
    let span = fun.mix(&arg);
    let t = Term::App { fun: Box::new(fun), arg: Box::new(arg) };
    Spanned::new(t, span)
  });
  let inline_num_oper = num_oper().then(term()).then(term()).map(|((op, fst), snd)| {
    let span = op.mix(&snd);
    let t = Term::Opx { op, fst: Box::new(fst), snd: Box::new(snd) };
    Spanned::new(t, span)
  });

  let lhs = name().then(pattern().repeated().collect()).boxed();
  let lhs = choice((lhs.clone(), lhs.delimited_by(just(Token::LParen), just(Token::RParen))));

  let rhs = choice((inline_num_oper, inline_app));

  lhs
    .then_ignore(just(Token::NewLine).repeated())
    .then_ignore(just(Token::Equals))
    .then_ignore(just(Token::NewLine).repeated())
    .then(rhs)
    .map(|((name, pats), body)| (name, Rule { def_id: DefId(0), pats, body }))
}

fn book<'a, I>() -> impl Parser<'a, I, DefinitionBook, extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  fn rules_to_book(
    rules: Vec<((Name, Rule), SimpleSpan)>,
    _span: SimpleSpan,
    emitter: &mut Emitter<Rich<Token>>,
  ) -> DefinitionBook {
    let mut book = DefinitionBook::new();

    // Check for repeated defs (could be rules out of order or actually repeated names)
    // TODO: Solve the lifetime here to avoid cloning names
    for (_, rules_data) in rules.into_iter().group_by(|((name, _), _)| name.clone()).into_iter() {
      let (rules, spans): (Vec<(Name, Rule)>, Vec<SimpleSpan>) = rules_data.unzip();
      let (mut names, mut rules): (Vec<Name>, Vec<Rule>) = rules.into_iter().unzip();
      let name = names.pop().unwrap();
      if !book.def_names.contains_name(&name) {
        let def_id = book.def_names.insert(name);
        rules.iter_mut().for_each(|rule| rule.def_id = def_id);
        book.defs.push(Definition { def_id, rules });
      } else {
        let span = SimpleSpan::new(spans.first().unwrap().start, spans.last().unwrap().end);
        emitter.emit(Rich::custom(span, format!("Repeated definition '{}'", name)));
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
    .collect::<Vec<((Name, Rule), SimpleSpan)>>();

  parsed_rules.validate(rules_to_book)
}
