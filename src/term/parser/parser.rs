use crate::term::{
  parser::lexer::{LexingError, Token},
  Adt, Book, Definition, MatchNum, Name, Op, Pattern, Rule, Tag, Term,
};
use chumsky::{
  error::{Error, RichReason},
  extra,
  input::{Emitter, SpannedInput, Stream, ValueInput},
  prelude::{Input, Rich},
  primitive::{any, choice, just},
  recursive::recursive,
  select,
  span::SimpleSpan,
  util::MaybeRef,
  IterParser, Parser,
};
use indexmap::{map::Entry, IndexMap};
use itertools::fold;
use logos::{Logos, SpannedIter};
use std::{iter::Map, ops::Range, path::Path};

// hvml grammar description:
// <Book>    ::= <TopLevel>*
// <TopLevel> ::= (<Def> | <Data>)
// <Def>     ::= <Rule> (<Rule>)*
// <Data>    ::= "data" <Name> "=" (<Name> | "(" <Name> (<Name>)* ")")+
// <Rule>    ::= ("(" <Name> <Pattern>* ")" | <Name> <Pattern>*) "=" (<InlineNumOp> | <InlineApp>)
// <Pattern> ::= "(" <Name> <Pattern>* ")" | <NameEra> | <Number>
// <Term>    ::= <Var> | <GlobalVar> | <Number> | <Lam> | <GlobalLam> | <Dup> | <Tup> | <Let> | <Match> | <NumOp> | <App>
// <Lam>     ::= ("λ"|"@") <NameEra> <Term>
// <GlobalLam> ::= ("λ"|"@") "$" <Name> <Term>
// <Dup>    ::= "dup" <Tag>? <NameEra> <NameEra> "=" <Term> ";" <Term>
// <Tup>    ::= "(" <Term> "," <Term> ")"
// <Let>    ::= "let" <LetPat> "=" <Term> ";" <Term>
// <LetPat> ::= <Name> | "(" <NameEra> "," <NameEra> ")"
// <Match>  ::= "match" (<Term> | <Name> "=" <Term>) "{" <match_arm>+ "}"
// <match_arm> ::= "|"? <Pattern> ":" <Term> ";"?
// <NumOp>  ::= "(" <numop_token> <Term> <Term> ")"
// <App>    ::= "(" <Term> (<Term>)* ")"
// <Var>    ::= <Name>
// <GlobalVar> ::= "$" <Name>
// <NameEra> ::= <Name> | "*"
// <Name>   ::= <name_token> // [_a-zA-Z][_a-zA-Z0-9]{0..7}
// <Number> ::= <number_token> // [0-9]+
// <Tag>    ::= "#" <Name>

pub fn parse_book(
  code: &str,
  default_book: impl Fn() -> Book,
  builtin: bool,
) -> Result<Book, Vec<Rich<Token>>> {
  book(default_book, builtin).parse(token_stream(code)).into_result()
}

pub fn parse_term(code: &str) -> Result<Term, Vec<Rich<Token>>> {
  // TODO: Make a function that calls a parser. I couldn't figure out how to type it correctly.
  term().parse(token_stream(code)).into_result()
}

/// Converts a Chumsky parser error into a message.
pub fn error_to_msg(err: &Rich<'_, Token>, code: &str, path: &Path) -> String {
  let Range { start, end } = err.span().into_range();
  let (lin, col) = line_and_col_of_byte(start, code);
  let reason = match err.reason() {
    // When many reasons, the first one is the most relevant.
    // Otherwise we just get 'multiple errors'.
    RichReason::Many(errs) => &errs[0],
    _ => err.reason(),
  };
  let path = format!("{}:{lin}:{col}", path.display());
  format!("At {}: {}\n{}", path, reason, highlight_error::highlight_error(usize::min(start, end), end, code))
}

fn line_and_col_of_byte(until: usize, src: &str) -> (usize, usize) {
  // Line and column numbers starts at 1.
  let mut line = 1;
  let mut col = 1;
  let mut gone = 0;
  for char in src.chars() {
    if gone >= until {
      break;
    }
    let char_len = char.len_utf8();
    gone += char_len;
    if char == '\n' {
      line += 1;
      col = 1;
    } else {
      col += char_len;
    }
  }
  (line, col)
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
  // FIXME: bug with chunsky when using with `.repeated`
  // select!(Token::Name(name) => Name::from(name)).labelled("<Name>")

  any()
    .filter(|t| matches!(t, Token::Name(_)))
    .map(|t| {
      let Token::Name(name) = t else { unreachable!() };
      Name::from(name)
    })
    .labelled("<Name>")
}

/// A top level name that not accepts `-`.
fn tl_name<'a, I>() -> impl Parser<'a, I, Name, extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  any()
    .filter(|t| matches!(t, Token::Name(_)))
    .map(|t| {
      let Token::Name(name) = t else { unreachable!() };
      name
    })
    .validate(|out, e, emitter| {
      if out.contains('-') {
        emitter.emit(Rich::custom(e, "Names with '-' are not supported at top level."));
      }
      Name::from(out)
    })
    .labelled("<Name>")
}

fn tag<'a, I>(default: Tag) -> impl Parser<'a, I, Tag, extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  just(Token::Hash).ignore_then(name()).or_not().map(move |x| x.map_or_else(|| default.clone(), Tag::Named))
}

fn name_or_era<'a, I>() -> impl Parser<'a, I, Option<Name>, extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  choice((any().filter(|a| matches!(a, Token::Asterisk)).map(|_| None), name().map(Some)))
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
    Token::Lte => Op::LTE,
    Token::Gte => Op::GTE,
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

  let number = select!(Token::Num(num) => Term::Num{val: num}).or(
    select!(Token::Error(LexingError::InvalidNumberLiteral) => ()).validate(|_, e, emit| {
      emit.emit(Rich::custom(e, "found invalid number literal expected number"));
      Term::Num { val: 0 }
    }),
  );

  let term_sep = just(Token::Semicolon).or_not();

  recursive(|term| {
    // *
    let era = just(Token::Asterisk).to(Term::Era).boxed();

    // #tag? λx body
    let lam = tag(Tag::Static)
      .then_ignore(just(Token::Lambda))
      .then(name_or_era())
      .then(term.clone())
      .map(|((tag, name), body)| Term::Lam { tag, nam: name, bod: Box::new(body) })
      .boxed();

    // #tag? λ$x body
    let global_lam = tag(Tag::Static)
      .then_ignore(just(Token::Lambda))
      .then(just(Token::Dollar).ignore_then(name()))
      .then(term.clone())
      .map(|((tag, name), body)| Term::Chn { tag, nam: name, bod: Box::new(body) })
      .boxed();

    // #tag {fst snd}
    let sup = tag(Tag::Auto)
      .then_ignore(just(Token::LBracket))
      .then(term.clone())
      .then(term.clone())
      .then_ignore(just(Token::RBracket))
      .map(|((tag, fst), snd)| Term::Sup { tag, fst: Box::new(fst), snd: Box::new(snd) })
      .boxed();

    // let #tag? {x1 x2} = body; next
    let dup = just(Token::Let)
      .ignore_then(tag(Tag::Auto))
      .then_ignore(just(Token::LBracket))
      .then(name_or_era())
      .then(name_or_era())
      .then_ignore(just(Token::RBracket))
      .then_ignore(just(Token::Equals))
      .then(term.clone())
      .then_ignore(term_sep.clone())
      .then(term.clone())
      .map(|((((tag, fst), snd), val), next)| Term::Dup {
        tag,
        fst,
        snd,
        val: Box::new(val),
        nxt: Box::new(next),
      })
      .boxed();

    // let a = ...
    // let (a, b) = ...
    let let_ = just(Token::Let)
      .ignore_then(pattern().validate(|pat, e, emit| {
        if matches!(&pat, Pattern::Num(..)) {
          emit.emit(Rich::custom(e, "Numbers not supported in let."));
        }
        pat
      }))
      .then_ignore(just(Token::Equals))
      .then(term.clone())
      .then_ignore(term_sep.clone())
      .then(term.clone())
      .map(|((pat, val), nxt)| Term::Let { pat, val: Box::new(val), nxt: Box::new(nxt) })
      .boxed();

    // '|'? pat: term
    let match_arm = just(Token::Or)
      .or_not()
      .ignore_then(pattern().then_ignore(just(Token::Colon)).then(term.clone()).boxed());

    // match (scrutinee | <name> = value) { pat: term;... }
    let match_ = just(Token::Match)
      .ignore_then(name().then_ignore(just(Token::Equals)).or_not())
      .then(term.clone())
      .then_ignore(just(Token::LBracket))
      .then(match_arm.separated_by(term_sep.clone()).allow_trailing().collect())
      .then_ignore(just(Token::RBracket))
      .map(|((bind, scrutinee), arms)| match bind {
        Some(nam) => Term::Let {
          pat: Pattern::Var(Some(nam.clone())),
          val: Box::new(scrutinee),
          nxt: Box::new(Term::Mat { matched: Box::new(Term::Var { nam }), arms }),
        },
        None => Term::Mat { matched: Box::new(scrutinee), arms },
      })
      .boxed();

    // #tag? (f arg1 arg2 ...)
    let app = tag(Tag::Static)
      .then_ignore(just(Token::LParen))
      .then(term.clone())
      .foldl(term.clone().repeated(), |(tag, fun), arg| {
        (tag.clone(), Term::App { tag, fun: Box::new(fun), arg: Box::new(arg) })
      })
      .then_ignore(just(Token::RParen))
      .map(|(_, app)| app)
      .boxed();

    let num_op = num_oper()
      .then(term.clone())
      .then(term.clone())
      .delimited_by(just(Token::LParen), just(Token::RParen))
      .map(|((op, fst), snd)| Term::Opx { op, fst: Box::new(fst), snd: Box::new(snd) })
      .boxed();

    // (x, ..n)
    let tup = term
      .clone()
      .separated_by(just(Token::Comma))
      .at_least(2)
      .collect::<Vec<Term>>()
      .map(|xs| make_tup_tree(&xs, |a, b| Term::Tup { fst: Box::new(a), snd: Box::new(b) }))
      .delimited_by(just(Token::LParen), just(Token::RParen))
      .boxed();

    let str = select!(Token::Str(s) => Term::Str { val: s }).boxed();
    let chr = select!(Token::Char(c) => Term::Num { val: c }).boxed();

    let list = term
      .clone()
      .separated_by(just(Token::Comma).or_not())
      .collect()
      .delimited_by(just(Token::LBrace), just(Token::RBrace))
      .map(|els| Term::Lst { els })
      .boxed();

    choice((
      global_var, var, number, list, str, chr, sup, tup, global_lam, lam, dup, let_, match_, num_op, app, era,
    ))
  })
}

fn make_tup_tree<A: Clone>(xs: &[A], make: fn(A, A) -> A) -> A {
  match xs {
    [] => unreachable!(),
    [x] => x.clone(),
    xs => {
      let half = xs.len() / 2;
      let (x, y) = xs.split_at(half);
      make(make_tup_tree(x, make), make_tup_tree(y, make))
    }
  }
}

fn pattern<'a, I>() -> impl Parser<'a, I, Pattern, extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  recursive(|pattern| {
    let var = name_or_era().map(Pattern::Var).boxed();

    let ctr = tl_name()
      .then(pattern.clone().repeated().collect())
      .map(|(nam, xs)| Pattern::Ctr(nam, xs))
      .delimited_by(just(Token::LParen), just(Token::RParen))
      .boxed();

    let tup = pattern
      .clone()
      .separated_by(just(Token::Comma))
      .at_least(2)
      .collect::<Vec<Pattern>>()
      .delimited_by(just(Token::LParen), just(Token::RParen))
      .map(|xs| make_tup_tree(&xs, |a, b| Pattern::Tup(Box::new(a), Box::new(b))))
      .boxed();

    let list = pattern
      .clone()
      .separated_by(just(Token::Comma).or_not())
      .collect()
      .delimited_by(just(Token::LBrace), just(Token::RBrace))
      .map(Pattern::Lst)
      .boxed();

    let zero =
      any().filter(|t| matches!(t, Token::Num(0))).map(|_| Pattern::Num(MatchNum::Zero)).labelled("0");

    let succ = just(Token::Add)
      .ignore_then(name_or_era().or_not())
      .map(|nam| Pattern::Num(MatchNum::Succ(nam)))
      .labelled("+")
      .boxed();

    choice((zero, succ, var, ctr, list, tup))
  })
}

fn rule_pattern<'a, I>() -> impl Parser<'a, I, (Name, Vec<Pattern>), extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  let lhs = tl_name().then(pattern().repeated().collect()).boxed();

  let just_lhs = lhs.clone().then_ignore(just(Token::Equals).map_err(|err: Rich<'a, Token>| {
    Error::<I>::expected_found(
      [
        Some(MaybeRef::Val(Token::Add)),
        Some(MaybeRef::Val(Token::LParen)),
        Some(MaybeRef::Val(Token::LBrace)),
        Some(MaybeRef::Val(Token::Equals)),
      ],
      None,
      *err.span(),
    )
  }));

  let paren_lhs = lhs
    .clone()
    .delimited_by(just(Token::LParen), just(Token::RParen))
    .then_ignore(just(Token::Equals).map_err(|err| map_unexpected_eof::<I>(err, Token::Equals)));

  choice((just_lhs, paren_lhs))
}

fn rule<'a, I>() -> impl Parser<'a, I, TopLevel, extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  let unclosed_terms =
    term().and_is(rule_pattern().not().rewind()).repeated().at_least(1).collect::<Vec<Term>>().boxed();

  rule_pattern()
    .then(term()
      // FIXME: This is used to report a parsing error that would be unclear otherwise
      // couldn't implement it in terms of `.recover(via_parser(...))`
      .then(unclosed_terms.or_not()).validate(
        |(body, unclosed_terms), e, emit| match unclosed_terms {
          Some(t) => {
            emit.emit(Rich::custom(e, "Missing Parenthesis around rule body"));
            fold(t, body, Term::app)
          }
          None => body,
        },
      ))
    .map(move |((name, pats), body)| TopLevel::Rule((name, Rule { pats, body })))
}

fn datatype<'a, I>() -> impl Parser<'a, I, TopLevel, extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  let arity_0 = tl_name().map_with_span(|nam, e| ((nam, vec![]), e));
  let arity_n = tl_name()
    .then(name().repeated().collect::<Vec<_>>())
    .delimited_by(just(Token::LParen), just(Token::RParen))
    .map_with_span(|(nam, args), e| ((nam, args), e));

  let ctrs = arity_0.or(arity_n).separated_by(just(Token::Or)).at_least(1).collect();
  let data_name = tl_name().map_with_span(|name, e| (name, e));

  just(Token::Data)
    .ignore_then(data_name.map_err(|err| map_unexpected_eof::<I>(err, Token::Name("<Name>".to_string()))))
    .then_ignore(just(Token::Equals))
    .then(ctrs.map_err(|err| map_unexpected_eof::<I>(err, Token::Name("constructor".to_string()))))
    .map(move |(name, ctrs)| TopLevel::Adt(name, ctrs))
}

fn map_unexpected_eof<'a, I>(err: Rich<'a, Token>, expected_token: Token) -> Rich<'a, Token>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  if err.found().is_none() {
    // Not using Error::expected_found here to not merge with others expected_found errors
    Rich::custom(*err.span(), format!("found end of input expected {}", expected_token))
  } else {
    err
  }
}

fn book<'a, I>(
  default_book: impl Fn() -> Book,
  builtin: bool,
) -> impl Parser<'a, I, Book, extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  let top_level = choice((datatype(), rule()));
  top_level
    .repeated()
    .collect()
    .validate(move |program, _, emit| collect_book(default_book(), program, builtin, emit))
}

/// Collect rules and adts into a book
fn collect_book(
  mut book: Book,
  program: Vec<TopLevel>,
  builtin: bool,
  emit: &mut Emitter<Rich<'_, Token>>,
) -> Book {
  for top_level in program {
    match top_level {
      TopLevel::Rule((name, rule)) => {
        if let Some(def) = book.defs.get_mut(&name) {
          def.rules.push(rule);
        } else {
          book.defs.insert(name.clone(), Definition { name, rules: vec![rule], builtin });
        }
      }
      TopLevel::Adt((nam, nam_span), adt) => match book.adts.get(&nam) {
        None => {
          let (ctrs, spans): (IndexMap<_, _>, Vec<_>) = adt.into_iter().unzip();

          for ((ctr, _), span) in ctrs.iter().zip(spans.into_iter()) {
            match book.ctrs.entry(ctr.clone()) {
              Entry::Vacant(e) => _ = e.insert(nam.clone()),
              Entry::Occupied(e) => emit.emit(Rich::custom(
                span,
                if book.adts[e.get()].builtin {
                  format!("{} is a built-in constructor and should not be overridden.", e.key())
                } else {
                  format!("Repeated constructor '{}'", e.key())
                },
              )),
            }
          }

          let adt = Adt { ctrs, builtin };
          book.adts.insert(nam.clone(), adt);
        }
        Some(adt) => emit.emit(Rich::custom(
          nam_span,
          if adt.builtin {
            format!("{} is a built-in datatype and should not be overridden.", nam)
          } else {
            format!("Repeated datatype '{}'", nam)
          },
        )),
      },
    }
  }
  book
}

enum TopLevel {
  Rule((Name, Rule)),
  Adt((Name, SimpleSpan), Vec<((Name, Vec<Name>), SimpleSpan)>),
}
