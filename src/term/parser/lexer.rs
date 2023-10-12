use logos::{FilterResult, Lexer, Logos};
use std::fmt;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(error=LexingError)]
pub enum Token {
  #[regex("[_.a-zA-Z][_.a-zA-Z0-9]*", |lex| lex.slice().parse().ok())]
  Name(String),

  #[regex("@|λ")]
  Lambda,

  #[token("$")]
  Dollar,

  #[token("let")]
  Let,

  #[token("dup")]
  Dup,

  #[token("if")]
  If,

  #[token("=")]
  Equals,

  #[regex("[0-9]+", |lex| lex.slice().parse().ok())]
  Num(u32),

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

  // #[token("<=")]
  // Lte,
  #[token(">")]
  Gtn,

  // #[token(">=")]
  // Gte,
  #[token("==")]
  EqualsEquals,

  #[token("!=")]
  NotEquals,

  #[token(";")]
  Semicolon,

  #[token("(")]
  LParen,

  #[token(")")]
  RParen,

  #[token("\n")]
  NewLine,

  #[regex("//.*", logos::skip)]
  SingleLineComment,

  #[token("/*", comment)]
  MultiLineComment,

  #[regex(r"[ \t\f\r]+", logos::skip)]
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
      Self::Name(s) => write!(f, "{}", s),
      Self::Lambda => write!(f, "λ"),
      Self::Dollar => write!(f, "$"),
      Self::Let => write!(f, "let"),
      Self::Dup => write!(f, "dup"),
      Self::If => write!(f, "if"),
      Self::Equals => write!(f, "="),
      Self::Num(num) => write!(f, "{num}"),
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
      // Self::Lte => write!(f, "<="),
      Self::Gtn => write!(f, ">"),
      // Self::Gte => write!(f, ">="),
      Self::NotEquals => write!(f, "!="),
      Self::EqualsEquals => write!(f, "=="),
      Self::Semicolon => write!(f, ";"),
      Self::LParen => write!(f, "("),
      Self::RParen => write!(f, ")"),
      Self::NewLine => write!(f, "<NewLine>"),
      Self::SingleLineComment => write!(f, "<SingleLineComment>"),
      Self::MultiLineComment => write!(f, "<MultiLineComment>"),
      Self::Whitespace => write!(f, "<Whitespace>"),
      Self::Error(LexingError::InvalidCharacter) => write!(f, "<InvalidCharacter>"),
      Self::Error(LexingError::UnclosedComment) => write!(f, "<UnclosedComment>"),
    }
  }
}
