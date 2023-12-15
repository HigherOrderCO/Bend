use logos::{FilterResult, Lexer, Logos};
use std::{fmt, num::ParseIntError};

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

  #[regex("[0-9_]+", |lex| from_radix(10, lex).ok(), priority = 2)]
  #[regex("0[xX][0-9a-fA-F_]+", |lex| from_radix(16, lex).ok())]
  #[regex("0[bB][0-1_]+", |lex| from_radix(2, lex).ok())]
  Num(u64),

  #[regex(r#""([^"\\]|\\t|\\u|\\n|\\")*""#, |lex| normalized_string(lex).ok())]
  Str(String),

  #[regex(r#"'[\W|\w]'"#, num_of_char)]
  Char(u64),

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

fn from_radix(radix: u32, lexer: &mut Lexer<Token>) -> Result<u64, ParseIntError> {
  let slice = if radix == 10 { lexer.slice() } else { &lexer.slice()[2 ..] };
  let slice = &slice.replace('_', "");
  u64::from_str_radix(slice, radix)
}

fn normalized_string(lexer: &mut Lexer<Token>) -> Result<String, ParseIntError> {
  let slice = lexer.slice();
  let slice = &slice[1 .. slice.len() - 1];

  let mut s = String::new();
  let chars = &mut slice.chars();

  while let Some(char) = chars.next() {
    match char {
      '\\' => match chars.next() {
        Some('n') => s.push('\n'),
        Some('t') => s.push('\t'),
        Some('u') => {
          let hex = chars.take(4).collect::<String>();
          let hex_val = u32::from_str_radix(&hex, 16)?;
          let char = char::from_u32(hex_val).unwrap_or(char::REPLACEMENT_CHARACTER);
          s.push(char);
        }
        Some(other) => {
          s.push('\\');
          s.push(other);
        }
        None => s.push('\\'),
      },
      other => s.push(other),
    }
  }
  Ok(s)
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

fn num_of_char(lexer: &mut Lexer<Token>) -> Option<u64> {
  let slice = lexer.slice();
  let slice = &slice[1 .. slice.len() - 1];
  let c = slice.chars().next()?;
  Some(u64::from(c))
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
      Self::Str(s) => write!(f, "\"{s}\""),
      Self::Char(c) => write!(f, "'{c}'"),
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
