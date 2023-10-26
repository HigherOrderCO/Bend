use super::lexer::{LexingError, Token};
use crate::term::{Adt, Book, Definition, LetPat, Name, Op, Rule, RulePat, Term};
use chumsky::{
  extra,
  input::{SpannedInput, Stream, ValueInput},
  prelude::{Input, Rich},
  primitive::{choice, just},
  recursive::recursive,
  select,
  span::SimpleSpan,
  IterParser, Parser,
};
use logos::{Logos, SpannedIter};
use std::{iter::Map, ops::Range};

/// <Book>   ::= <Def>* // Sequential rules grouped by name
/// <Def>    ::= \n* <Rule> (\n+ <Rule>)* \n*
/// <Rule>   ::= ("(" <Name> <Pattern>* ")" | <Name> <Pattern>*) \n* "=" \n* (<InlineNumOp> | <InlineApp>)
/// <Pattern> ::= "(" <Name> <Pattern>* ")" | <NameEra> | <Number>
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
pub fn parse_definition_book(code: &str) -> Result<Book, Vec<Rich<Token>>> {
  book().parse(token_stream(code)).into_result()
}

pub fn parse_term(code: &str) -> Result<Term, Vec<Rich<Token>>> {
  // TODO: Make a function that calls a parser. I couldn't figure out how to type it correctly.
  term().parse(token_stream(code)).into_result()
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
  let var = name().map(|name| Term::Var { nam: name }).boxed();
  let global_var = just(Token::Dollar).ignore_then(name()).map(|name| Term::Lnk { nam: name }).boxed();
  let number = select!(Token::Num(num) => Term::Num{val: num});
  let term_sep = just(Token::Semicolon).or_not();

  recursive(|term| {
    // λx body
    let lam = just(Token::Lambda)
      .ignore_then(name_or_era())
      .then(term.clone())
      .map(|(name, body)| Term::Lam { nam: name, bod: Box::new(body) })
      .boxed();

    // λ$x body
    let global_lam = just(Token::Lambda)
      .ignore_then(just(Token::Dollar))
      .ignore_then(name())
      .then(term.clone())
      .map(|(name, body)| Term::Chn { nam: name, bod: Box::new(body) })
      .boxed();

    // dup x1 x2 = body; next
    let dup = just(Token::Dup)
      .ignore_then(name_or_era())
      .then(name_or_era())
      .then_ignore(just(Token::Equals))
      .then(term.clone())
      .then_ignore(term_sep.clone())
      .then(term.clone())
      .map(|(((fst, snd), val), next)| Term::Dup { fst, snd, val: Box::new(val), nxt: Box::new(next) })
      .boxed();

    // (x, y)
    let pair = term
      .clone()
      .then_ignore(just(Token::Comma))
      .then(term.clone())
      .delimited_by(just(Token::LParen), just(Token::RParen))
      .map(|(fst, snd)| Term::Tup { fst: Box::new(fst), snd: Box::new(snd) })
      .boxed();

    // let a = ...
    // let (a, b) = ...
    let let_ = just(Token::Let)
      .ignore_then(let_pat())
      .then_ignore(just(Token::Equals))
      .then(term.clone())
      .then_ignore(term_sep.clone())
      .then(term.clone())
      .map(|((pat, val), nxt)| Term::Let { pat, val: Box::new(val), nxt: Box::new(nxt) })
      .boxed();

    // match val { 0: zero; 1 + pred: succ }
    let match_ = just(Token::Match)
      .ignore_then(term.clone())
      .then_ignore(just(Token::LBracket))
      .then_ignore(select!(Token::Num(0) => ()))
      .then_ignore(just(Token::Colon))
      .then(term.clone())
      .then_ignore(term_sep.clone())
      .then_ignore(select!(Token::Num(1) => ()))
      .then_ignore(just(Token::Add))
      .then(name_or_era())
      .then_ignore(just(Token::Colon))
      .then(term.clone())
      .then_ignore(just(Token::RBracket))
      .map(|(((cond, zero), pred), succ)| Term::Match {
        cond: Box::new(cond),
        zero: Box::new(zero),
        succ: Box::new(Term::Lam { nam: pred, bod: Box::new(succ) }),
      })
      .boxed();

    // (f arg1 arg2 ...)
    let app = term
      .clone()
      .foldl(term.clone().repeated(), |fun, arg| Term::App { fun: Box::new(fun), arg: Box::new(arg) })
      .delimited_by(just(Token::LParen), just(Token::RParen))
      .boxed();

    let num_op = num_oper()
      .then(term.clone())
      .then(term.clone())
      .delimited_by(just(Token::LParen), just(Token::RParen))
      .map(|((op, fst), snd)| Term::Opx { op, fst: Box::new(fst), snd: Box::new(snd) })
      .boxed();

    choice((global_var, var, number, pair, global_lam, lam, dup, let_, match_, num_op, app))
  })
}

fn let_pat<'a, I>() -> impl Parser<'a, I, LetPat, extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  let pat_nam = name().map(|nam| LetPat::Var(nam)).boxed();

  let pat_tup = name_or_era()
    .then_ignore(just(Token::Comma))
    .then(name_or_era())
    .delimited_by(just(Token::LParen), just(Token::RParen))
    .map(|(fst, snd)| LetPat::Tup(fst, snd))
    .boxed();

  choice((pat_nam, pat_tup))
}

fn rule_pat<'a, I>() -> impl Parser<'a, I, RulePat, extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  recursive(|rule_pat| {
    let var = name().map(RulePat::Var).boxed();

    let ctr = name()
      .then(rule_pat.clone().repeated().collect())
      .map(|(nam, xs)| RulePat::Ctr(nam, xs))
      .delimited_by(just(Token::LParen), just(Token::RParen))
      .boxed();

    choice((var, ctr))
  })
}

fn rule<'a, I>() -> impl Parser<'a, I, (Name, Rule), extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  let lhs = name().then(rule_pat().repeated().collect()).boxed();
  let lhs = choice((lhs.clone(), lhs.clone().delimited_by(just(Token::LParen), just(Token::RParen))));

  lhs.then_ignore(just(Token::Equals)).then(term()).map(|((name, pats), body)| (name, Rule { pats, body }))
}

fn datatype<'a, I>() -> impl Parser<'a, I, (Name, Adt), extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  let arity_0 = name().map(|nam| (nam, 0));
  let arity_n = name()
    .then(name().repeated().collect::<Vec<_>>())
    .delimited_by(just(Token::LParen), just(Token::RParen))
    .map(|(nam, args)| (nam, args.len()));
  let ctr = arity_0.or(arity_n).separated_by(just(Token::Or));

  just(Token::Data)
    .ignore_then(name())
    .then_ignore(just(Token::Equals))
    .then(ctr.collect::<Vec<(Name, usize)>>())
    .map(|(name, ctrs)| (name, Adt { ctrs: ctrs.into_iter().collect() }))
}

fn book<'a, I>() -> impl Parser<'a, I, Book, extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  let top_level = choice((rule().map(|x| TopLevel::Rule(x)), datatype().map(|x| TopLevel::Adt(x))));

  top_level.repeated().collect::<Vec<_>>().try_map(|program, span| {
    let mut book = Book::new();

    for top_level in program {
      match top_level {
        TopLevel::Rule((nam, rule)) => {
          if let Some(def_id) = book.def_names.def_id(&nam) {
            book.defs.get_mut(&def_id).unwrap().rules.push(rule);
          } else {
            let def_id = book.def_names.insert(nam);
            book.defs.insert(def_id, Definition { def_id, rules: vec![rule] });
          }
        }
        TopLevel::Adt((nam, adt)) => {
          if !book.adts.contains_key(&nam) {
            book.adts.insert(nam.clone(), adt.clone());
            for (ctr, _) in adt.ctrs {
              if !book.ctrs.contains_key(&ctr) {
                book.ctrs.insert(ctr, nam.clone());
              } else {
                return Err(Rich::custom(
                  SimpleSpan::from(0 .. 4),
                  format!("Repeated constructor '{}'", nam),
                ));
              }
            }
          } else {
            return Err(Rich::custom(span, format!("Repeated datatype '{}'", nam)));
          }
        }
      }
    }
    Ok(book)
  })
}

enum TopLevel {
  Rule((Name, Rule)),
  Adt((Name, Adt)),
}
