use logos::{FilterResult, Lexer, Logos};
use std::fmt;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(error=LexingError)]
pub enum Token {
  #[regex("[_.a-zA-Z][_.a-zA-Z0-9]*", |lex| lex.slice().parse().ok())]
  Name(String),

  #[regex("[_.a-zA-Z][_.a-zA-Z0-9]*-1", |lex| lex.slice().parse().ok())]
  Pred(String),

  #[regex("@|λ")]
  Lambda,

  #[token("$")]
  Dollar,

  #[token("let")]
  Let,

  #[token("dup")]
  Dup,

  #[token("match")]
  Match,

  #[token("=")]
  Equals,

  #[regex("[0-9]+", |lex| lex.slice().parse().ok())]
  Num(u64),

  #[token("#")]
  Hash,

  #[token("+")]
  Add,

  #[token("-")]
  Sub,

  #[token("*")]
  Asterisk,

  #[token("/")]
  Div,

  #[token("%")]
  Mod,

  #[token("~")]
  Tilde,

  #[token("&")]
  And,

  #[token("|")]
  Or,

  #[token("^")]
  Xor,

  #[token("<<")]
  Shl,

  #[token(">>")]
  Shr,

  #[token("<")]
  Ltn,

  #[token(">")]
  Gtn,

  #[token("<=")]
  Lte,

  #[token(">=")]
  Gte,

  #[token("==")]
  EqualsEquals,

  #[token("!=")]
  NotEquals,

  #[token(";")]
  Semicolon,

  #[token(":")]
  Colon,

  #[token(",")]
  Comma,

  #[token("(")]
  LParen,

  #[token(")")]
  RParen,

  #[token("{")]
  LBracket,

  #[token("}")]
  RBracket,

  #[regex("//.*", logos::skip)]
  SingleLineComment,

  #[token("/*", comment)]
  MultiLineComment,

  #[regex(r"[ \t\f\r\n]+", logos::skip)]
  Whitespace,

  Error(LexingError),
}

#[derive(Default, Debug, PartialEq, Clone)]
pub enum LexingError {
  UnclosedComment,

  #[default]
  InvalidCharacter,
}

// Lexer for nested multi-line comments
#[derive(Logos)]
pub enum MultiLineComment {
  #[token("/*")]
  Open,

  #[token("*/")]
  Close,

  #[regex("(?s).")]
  Other,
}

fn comment(lexer: &mut Lexer<'_, Token>) -> FilterResult<(), LexingError> {
  let start = lexer.remainder();
  let mut comment = MultiLineComment::lexer(start);
  let mut depth = 1; // Already matched an Open token, so count it
  loop {
    if let Some(token) = comment.next() {
      match token {
        Ok(MultiLineComment::Open) => depth += 1,
        Ok(MultiLineComment::Close) => depth -= 1,
        Ok(MultiLineComment::Other) => {}
        Err(()) => unreachable!(),
      }
    } else {
      // Unclosed comment
      return FilterResult::Error(LexingError::UnclosedComment);
    }
    if depth <= 0 {
      break;
    }
  }
  let end = comment.remainder();
  let span = (end as *const str as *const () as usize) - (start as *const str as *const () as usize);
  lexer.bump(span);
  FilterResult::Skip
}

impl fmt::Display for Token {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Self::Name(s) | Self::Pred(s) => write!(f, "{}", s),
      Self::Lambda => write!(f, "λ"),
      Self::Dollar => write!(f, "$"),
      Self::Let => write!(f, "let"),
      Self::Dup => write!(f, "dup"),
      Self::Match => write!(f, "match"),
      Self::Equals => write!(f, "="),
      Self::Num(num) => write!(f, "{num}"),
      Self::Hash => write!(f, "#"),
      Self::Add => write!(f, "+"),
      Self::Sub => write!(f, "-"),
      Self::Asterisk => write!(f, "*"),
      Self::Div => write!(f, "/"),
      Self::Mod => write!(f, "%"),
      Self::Tilde => write!(f, "~"),
      Self::And => write!(f, "&"),
      Self::Or => write!(f, "|"),
      Self::Xor => write!(f, "^"),
      Self::Shl => write!(f, "<<"),
      Self::Shr => write!(f, ">>"),
      Self::Ltn => write!(f, "<"),
      Self::Gtn => write!(f, ">"),
      Self::Lte => write!(f, "<="),
      Self::Gte => write!(f, ">="),
      Self::NotEquals => write!(f, "!="),
      Self::EqualsEquals => write!(f, "=="),
      Self::Colon => write!(f, ":"),
      Self::Comma => write!(f, ","),
      Self::Semicolon => write!(f, ";"),
      Self::LParen => write!(f, "("),
      Self::RParen => write!(f, ")"),
      Self::LBracket => write!(f, "{{"),
      Self::RBracket => write!(f, "}}"),
      Self::SingleLineComment => write!(f, "<SingleLineComment>"),
      Self::MultiLineComment => write!(f, "<MultiLineComment>"),
      Self::Whitespace => write!(f, "<Whitespace>"),
      Self::Error(LexingError::InvalidCharacter) => write!(f, "<InvalidCharacter>"),
      Self::Error(LexingError::UnclosedComment) => write!(f, "<UnclosedComment>"),
    }
  }
}
