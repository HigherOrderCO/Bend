use crate::term::{
  parser::lexer::{LexingError, Token},
  Adt, Book, MatchNum, Name, Op, Origin, Pattern, Rule, Tag, Term,
};
use chumsky::{
  error::RichReason,
  extra,
  input::{Emitter, SpannedInput, Stream, ValueInput},
  prelude::{Input, Rich},
  primitive::{choice, end, just},
  recursive::recursive,
  select,
  span::SimpleSpan,
  IterParser, Parser,
};
use logos::{Logos, SpannedIter};
use std::{collections::hash_map::Entry, iter::Map, ops::Range, path::Path};

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

pub fn parse_definition_book(
  code: &str,
  default_book: impl Fn() -> Book,
  rule_type: Origin,
) -> Result<Book, Vec<Rich<Token>>> {
  book(default_book, rule_type).parse(token_stream(code)).into_result()
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
  format!("At {}: {}\n{}", path, reason, highlight_error::highlight_error(start, end, code))
}

fn line_and_col_of_byte(until: usize, src: &str) -> (usize, usize) {
  let mut line = 1; // Line number starts at 1.
  let mut col = 0;
  let mut gone = 0;
  for char in src.chars() {
    let char_len = char.len_utf8();
    gone += char_len;
    if char == '\n' {
      line += 1;
      col = 0;
    } else {
      col += char_len;
    }
    if gone >= until {
      break;
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

fn soft_keyword<'a, I>(keyword: &'a str) -> impl Parser<'a, I, (), extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  select!(Token::Name(name) if name == keyword => ())
}

fn name<'a, I>() -> impl Parser<'a, I, Name, extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  select!(Token::Name(name) => Name(name))
}

/// A top level name that not accepts `-`.
fn tl_name<'a, I>() -> impl Parser<'a, I, Name, extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  select!(Token::Name(name) => Name(name)).validate(|out, span, emitter| {
    if out.contains('-') {
      emitter.emit(Rich::custom(span, "Names with '-' are not supported at top level."));
    }
    out
  })
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
  let number = select!(Token::Num(num) => Term::Num{val: num});
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
      .ignore_then(pattern().validate(|pat, span, emit| {
        if matches!(&pat, Pattern::Num(..)) {
          emit.emit(Rich::custom(span, "Numbers not supported in let."));
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
    let match_arm = just(Token::Or).or_not().ignore_then(
      pattern()
        .or(just(Token::Add).map(|_| Pattern::Num(MatchNum::Succ(None))))
        .then_ignore(just(Token::Colon))
        .then(term.clone())
        .boxed(),
    );

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
          nxt: Box::new(Term::Match { scrutinee: Box::new(Term::Var { nam }), arms }),
        },
        None => Term::Match { scrutinee: Box::new(scrutinee), arms },
      })
      .boxed();

    let native_match = just(Token::Match)
      .ignore_then(name())
      .then_ignore(just(Token::LBracket))
      .then_ignore(select!(Token::Num(0) => ()))
      .then_ignore(just(Token::Colon))
      .then(term.clone())
      .then_ignore(term_sep.clone())
      .then_ignore(just(Token::Add))
      .then_ignore(just(Token::Colon))
      .then(term.clone())
      .then_ignore(term_sep.clone())
      .then_ignore(just(Token::RBracket))
      .map(|((nam, zero), succ)| Term::Match {
        scrutinee: Box::new(Term::Var { nam }),
        arms: vec![(Pattern::Num(MatchNum::Zero), zero), (Pattern::Num(MatchNum::Succ(None)), succ)],
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
      .map(|els| Term::List { els })
      .boxed();

    choice((
      global_var,
      var,
      number,
      list,
      str,
      chr,
      sup,
      tup,
      global_lam,
      lam,
      dup,
      let_,
      native_match,
      match_,
      num_op,
      app,
      era,
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
      .map(Pattern::List)
      .boxed();

    let zero = select!(Token::Num(0) => Pattern::Num(MatchNum::Zero));

    let succ =
      just(Token::Add).ignore_then(name_or_era()).map(|nam| Pattern::Num(MatchNum::Succ(Some(nam)))).boxed();

    choice((zero, succ, var, ctr, list, tup))
  })
}

fn rule_pattern<'a, I>() -> impl Parser<'a, I, (Name, Vec<Pattern>), extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  let lhs = tl_name().then(pattern().repeated().collect()).boxed();
  choice((lhs.clone(), lhs.clone().delimited_by(just(Token::LParen), just(Token::RParen))))
    .then_ignore(just(Token::Equals))
}

/// This rule always emits an error when it parses successfully
/// It is used to report a parsing error that would be unclear otherwise
fn rule_body_missing_paren<'a, I>()
-> impl Parser<'a, I, ((Name, Vec<Pattern>), Term), extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  let terms = tag(Tag::Static)
    .then(term())
    .foldl(term().and_is(soft_keyword("data").not()).repeated().at_least(1), |(tag, fun), arg| {
      (tag.clone(), Term::App { tag, fun: Box::new(fun), arg: Box::new(arg) })
    });

  let end_of_rule = end().or(soft_keyword("data")).rewind();

  rule_pattern()
    .then(terms.validate(|terms, span, emit| {
      emit.emit(Rich::custom(span, format!("Missing Parenthesis around rule body")));
      terms
    }))
    .map(|(rule, (_, app))| (rule, app))
    .then_ignore(end_of_rule)
    .boxed()
}

fn rule<'a, I>(rule_type: Origin) -> impl Parser<'a, I, (Name, Rule), extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  rule_body_missing_paren()
    .or(rule_pattern().then(term()))
    .map(move |((name, pats), body)| (name, Rule { pats, body, origin: rule_type }))
}

fn datatype<'a, I>(origin: Origin) -> impl Parser<'a, I, (Name, Adt, SimpleSpan), extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  let arity_0 = tl_name().map(|nam| (nam, vec![]));
  let arity_n = tl_name()
    .then(name().repeated().collect::<Vec<_>>())
    .delimited_by(just(Token::LParen), just(Token::RParen))
    .map(|(nam, args)| (nam, args));
  let ctr = arity_0.or(arity_n);

  let data = soft_keyword("data");

  data
    .ignore_then(tl_name())
    .then_ignore(just(Token::Equals))
    .then(ctr.separated_by(just(Token::Or)).collect::<Vec<(Name, Vec<Name>)>>())
    .map_with_span(move |(name, ctrs), span| (name, Adt { ctrs: ctrs.into_iter().collect(), origin }, span))
}

fn book<'a, I>(
  default_book: impl Fn() -> Book,
  origin: Origin,
) -> impl Parser<'a, I, Book, extra::Err<Rich<'a, Token>>>
where
  I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
  let top_level = choice((datatype(origin).map(TopLevel::Adt), rule(origin).map(TopLevel::Rule)));

  top_level.repeated().collect().validate(move |program, _, emit| collect_book(default_book(), program, emit))
}

/// Collect rules and adts into a book
fn collect_book(mut book: Book, program: Vec<TopLevel>, emit: &mut Emitter<Rich<'_, Token>>) -> Book {
  for top_level in program {
    match top_level {
      TopLevel::Rule((nam, rule)) => {
        if let Some(def) = book.get_def_mut(&nam) {
          def.rules.push(rule);
        } else {
          book.insert_def(nam, vec![rule]);
        }
      }
      TopLevel::Adt((nam, adt, span)) => match book.adts.get(&nam) {
        None => {
          book.adts.insert(nam.clone(), adt.clone());
          for (ctr, _) in adt.ctrs {
            match book.ctrs.entry(ctr) {
              Entry::Vacant(e) => _ = e.insert(nam.clone()),
              Entry::Occupied(e) => emit.emit(Rich::custom(span, match book.adts[e.get()].origin {
                Origin::Builtin => {
                  format!("{} is a built-in constructor and should not be overridden.", e.key())
                }
                _ => format!("Repeated constructor '{}'", e.key()),
              })),
            }
          }
        }
        Some(adt) => emit.emit(Rich::custom(span, match adt.origin {
          Origin::Builtin => format!("{} is a built-in datatype and should not be overridden.", nam),
          _ => format!("Repeated datatype '{}'", nam),
        })),
      },
    }
  }
  book
}

enum TopLevel {
  Rule((Name, Rule)),
  Adt((Name, Adt, SimpleSpan)),
}
