use super::lexer::{LexingError, Token};
use crate::term::{DefId, Definition, DefinitionBook, Name, Op, Term};
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
use std::{iter::Map, ops::Range};

// use crate::ast::hvm_lang::Op;

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

pub fn parse_term(code: &str) -> Result<Term, Vec<Rich<Token>>> {
  let inline_app =
    term().foldl(term().repeated(), |fun, arg| Term::App { fun: Box::new(fun), arg: Box::new(arg) });
  let inline_num_oper = num_oper().then(term()).then(term()).map(|((op, fst), snd)| Term::Opx {
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

fn num_oper<'a, I>() -> impl Parser<'a, I, Op, extra::Err<Rich<'a, Token>>>
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
}

fn term<'a, I>() -> impl Parser<'a, I, Term, extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  let new_line = || just(Token::NewLine).repeated();
  let var = name().map(|name| Term::Var { nam: name }).boxed();
  let global_var = just(Token::Dollar).ignore_then(name()).map(|name| Term::Lnk { nam: name }).boxed();
  let term_sep = choice((just(Token::NewLine), just(Token::Semicolon)));

  let unsigned = select!(Token::Num(num) => Term::Num{val: num});

  recursive(|term| {
    // λx body
    let lam = just(Token::Lambda)
      .ignore_then(new_line())
      .ignore_then(name_or_era())
      .then_ignore(new_line())
      .then(term.clone())
      .map(|(name, body)| Term::Lam { nam: name, bod: Box::new(body) })
      .boxed();

    // λ$x body
    let global_lam = just(Token::Lambda)
      .ignore_then(new_line())
      .ignore_then(just(Token::Dollar))
      .ignore_then(new_line())
      .ignore_then(name())
      .then_ignore(new_line())
      .then(term.clone())
      .map(|(name, body)| Term::Chn { nam: name, bod: Box::new(body) })
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
      .then_ignore(term_sep)
      .then_ignore(new_line())
      .then(term.clone())
      .map(|((nam, val), nxt)| Term::Let { nam, val: Box::new(val), nxt: Box::new(nxt) })
      .boxed();

    let if_ = just(Token::If)
      .ignore_then(new_line())
      .ignore_then(term.clone())
      .then(
        term
          .clone()
          .delimited_by(new_line(), new_line())
          .delimited_by(just(Token::LBracket), just(Token::RBracket))
          .delimited_by(new_line(), new_line()),
      )
      .then_ignore(select!(Token::Name(nam) if nam == "else" => ()))
      .then(
        term
          .clone()
          .delimited_by(new_line(), new_line())
          .delimited_by(just(Token::LBracket), just(Token::RBracket))
          .delimited_by(new_line(), new_line()),
      )
      .map(|((cond, then), els_)| Term::If {
        cond: Box::new(cond),
        then: Box::new(then),
        els_: Box::new(els_),
      });

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
      .map(|((op, fst), snd)| Term::Opx { op, fst: Box::new(fst), snd: Box::new(snd) })
      .boxed();

    choice((global_var, var, unsigned, global_lam, lam, dup, let_, if_, num_op, app))
  })
}

fn definition<'a, I>() -> impl Parser<'a, I, (Name, Definition), extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  let inline_app =
    term().foldl(term().repeated(), |fun, arg| Term::App { fun: Box::new(fun), arg: Box::new(arg) });
  let inline_num_oper = num_oper().then(term()).then(term()).map(|((op, fst), snd)| Term::Opx {
    op,
    fst: Box::new(fst),
    snd: Box::new(snd),
  });

  let lhs = name().boxed();
  let lhs = choice((lhs.clone(), lhs.delimited_by(just(Token::LParen), just(Token::RParen))));

  let rhs = choice((inline_num_oper, inline_app));

  lhs
    .then_ignore(just(Token::NewLine).repeated())
    .then_ignore(just(Token::Equals))
    .then_ignore(just(Token::NewLine).repeated())
    .then(rhs)
    .map(|(name, body)| (name, Definition { def_id: DefId(0), body }))
}

fn book<'a, I>() -> impl Parser<'a, I, DefinitionBook, extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  fn definitions_to_book(
    defs: Vec<((Name, Definition), SimpleSpan)>,
    _span: SimpleSpan,
    emitter: &mut Emitter<Rich<Token>>,
  ) -> DefinitionBook {
    let mut book = DefinitionBook::new();

    // Check for repeated defs (could be rules out of order or actually repeated names)
    // TODO: Solve the lifetime here to avoid cloning names
    for ((name, Definition { body, .. }), def_span) in defs {
      if !book.def_names.contains_name(&name) {
        let def_id = book.def_names.insert(name);
        book.defs.push(Definition { def_id, body });
      } else {
        let (start, end) = (def_span.start, def_span.end);
        let span = SimpleSpan::new(start, end);
        emitter.emit(Rich::custom(span, format!("Repeated definition '{}'", name)));
      }
    }
    book
  }

  let new_line = just(Token::NewLine).repeated();

  let parsed_definitions = definition()
    .map_with_span(|def, span| (def, span))
    .separated_by(new_line.at_least(1))
    .allow_leading()
    .allow_trailing()
    .collect::<Vec<((Name, Definition), SimpleSpan)>>();

  parsed_definitions.validate(definitions_to_book)
}
