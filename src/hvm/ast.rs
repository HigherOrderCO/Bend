use highlight_error::highlight_error;
use std::collections::BTreeMap;
use std::fmt::Debug;
use TSPL::{new_parser, ParseError, Parser};

// Types
// -----

pub type Tag = u8;
pub type Val = u32;

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Numb(pub u32);

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum Tree {
  Var { nam: String },
  Sub { nam: String },
  Era,
  Del,
  Lam { fst: Box<Tree>, snd: Box<Tree> },
  App { fst: Box<Tree>, snd: Box<Tree> },
  Dup { fst: Box<Tree>, snd: Box<Tree> },
  Sup { fst: Box<Tree>, snd: Box<Tree> },
  Ref { nam: String },
  // Num { val: Numb },
  // Opr { fst: Box<Tree>, snd: Box<Tree> },
  // Swi { fst: Box<Tree>, snd: Box<Tree> },
}

pub type Redex = (bool, Tree, Tree);

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Net {
  pub root: Tree,
  pub rbag: Vec<Redex>,
}

pub struct Book {
  pub defs: BTreeMap<String, Net>,
}

// Parser
// ------

pub type ParseResult<T> = std::result::Result<T, ParseError>;

new_parser!(CoreParser);

impl<'i> CoreParser<'i> {
  pub fn parse_numb_sym(&mut self) -> ParseResult<Numb> {
    self.consume("[")?;

    // numeric casts
    if let Some(cast) = match () {
      _ if self.try_consume("u24") => Some(TY_U24),
      _ if self.try_consume("i24") => Some(TY_I24),
      _ if self.try_consume("f24") => Some(TY_F24),
      _ => None,
    } {
      // Casts can't be partially applied, so nothing should follow.
      self.consume("]")?;

      return Ok(Numb::new_sym(cast));
    }

    // Parses the symbol
    let op = Numb::new_sym(match () {
      // numeric operations
      _ if self.try_consume("+") => OP_ADD,
      _ if self.try_consume("-") => OP_SUB,
      _ if self.try_consume(":-") => FP_SUB,
      _ if self.try_consume("*") => OP_MUL,
      _ if self.try_consume("/") => OP_DIV,
      _ if self.try_consume(":/") => FP_DIV,
      _ if self.try_consume("%") => OP_REM,
      _ if self.try_consume(":%") => FP_REM,
      _ if self.try_consume("=") => OP_EQ,
      _ if self.try_consume("!") => OP_NEQ,
      _ if self.try_consume("<<") => OP_SHL,
      _ if self.try_consume(":<<") => FP_SHL,
      _ if self.try_consume(">>") => OP_SHR,
      _ if self.try_consume(":>>") => FP_SHR,
      _ if self.try_consume("<") => OP_LT,
      _ if self.try_consume(">") => OP_GT,
      _ if self.try_consume("&") => OP_AND,
      _ if self.try_consume("|") => OP_OR,
      _ if self.try_consume("^") => OP_XOR,
      _ => self.expected("operator symbol")?,
    });

    self.skip_trivia();
    // Syntax for partial operations, like `[*2]`
    let num =
      if self.peek_one() != Some(']') { Numb::partial(op, Numb(self.parse_numb_lit()?.0)) } else { op };

    // Closes symbol bracket
    self.consume("]")?;

    // Returns the symbol
    Ok(num)
  }

  pub fn parse_numb_lit(&mut self) -> ParseResult<Numb> {
    let ini = self.index;
    let num = self.take_while(|x| x.is_alphanumeric() || x == '+' || x == '-' || x == '.');
    let mut num_parser = CoreParser::new(num);
    let end = self.index;
    Ok(Numb(
      if num.contains('.') || num.contains("inf") || num.contains("NaN") {
        let val: f32 = num.parse().map_err(|err| {
          let msg = format!("invalid number literal: {}\n{}", err, highlight_error(ini, end, self.input));
          self.expected_and::<Numb>("number literal", &msg).unwrap_err()
        })?;
        Numb::new_f24(val)
      } else if num.starts_with('+') || num.starts_with('-') {
        *num_parser.index() += 1;
        let val = num_parser.parse_u64()? as i32;
        Numb::new_i24(if num.starts_with('-') { -val } else { val })
      } else {
        let val = num_parser.parse_u64()? as u32;
        Numb::new_u24(val)
      }
      .0,
    ))
  }

  pub fn parse_numb(&mut self) -> ParseResult<Numb> {
    self.skip_trivia();

    // Parses symbols (SYM)
    if let Some('[') = self.peek_one() {
      self.parse_numb_sym()
    // Parses numbers (U24,I24,F24)
    } else {
      self.parse_numb_lit()
    }
  }

  pub fn parse_tree(&mut self) -> ParseResult<Tree> {
    self.skip_trivia();
    //println!("aaa ||{}", &self.input[self.index..]);
    if self.starts_with("+(") {
      self.advance_many(2);
      let fst = Box::new(self.parse_tree()?);
      self.skip_trivia();
      let snd = Box::new(self.parse_tree()?);
      self.consume(")")?;
      Ok(Tree::Lam { fst, snd })
    } else if self.starts_with("-(") {
      self.advance_many(2);
      let fst = Box::new(self.parse_tree()?);
      self.skip_trivia();
      let snd = Box::new(self.parse_tree()?);
      self.consume(")")?;
      Ok(Tree::App { fst, snd })
    } else if self.starts_with("+{") {
      self.advance_many(2);
      let fst = Box::new(self.parse_tree()?);
      self.skip_trivia();
      let snd = Box::new(self.parse_tree()?);
      self.consume("}")?;
      Ok(Tree::Sup { fst, snd })
    } else if self.starts_with("-{") {
      self.advance_many(2);
      let fst = Box::new(self.parse_tree()?);
      self.skip_trivia();
      let snd = Box::new(self.parse_tree()?);
      self.consume("}")?;
      Ok(Tree::Dup { fst, snd })
    } else if self.starts_with("@") {
      self.advance_one();
      let nam = self.parse_name()?;
      Ok(Tree::Ref { nam })
    } else if self.starts_with("+*") {
      self.advance_many(2);
      Ok(Tree::Era)
    } else if self.starts_with("-*") {
      self.advance_many(2);
      Ok(Tree::Del)
    } else if self.starts_with("+") {
      self.advance_one();
      let nam = self.parse_name()?;
      Ok(Tree::Var { nam })
    } else if self.starts_with("-") {
      self.advance_one();
      let nam = self.parse_name()?;
      Ok(Tree::Sub { nam })
    } else {
      self.expected("tree")
    }
  }

  pub fn parse_net(&mut self) -> ParseResult<Net> {
    let root = self.parse_tree()?;
    let mut rbag = Vec::new();
    self.skip_trivia();
    while self.peek_one() == Some('&') {
      self.consume("&")?;
      let par = if let Some('!') = self.peek_one() {
        self.consume("!")?;
        true
      } else {
        false
      };
      let fst = self.parse_tree()?;
      self.consume("~")?;
      let snd = self.parse_tree()?;
      rbag.push((par, fst, snd));
      self.skip_trivia();
    }
    Ok(Net { root, rbag })
  }

  pub fn parse_book(&mut self) -> ParseResult<Book> {
    let mut defs = BTreeMap::new();
    while !self.is_eof() {
      self.consume("@")?;
      let name = self.parse_name()?;
      self.consume("=")?;
      let net = self.parse_net()?;
      defs.insert(name, net);
    }
    Ok(Book { defs })
  }

  fn try_consume(&mut self, str: &str) -> bool {
    let matches = self.peek_many(str.len()) == Some(str);
    if matches {
      self.advance_many(str.len());
    }
    matches
  }
}

// Stringifier
// -----------

impl Numb {
  pub fn show(&self) -> String {
    let numb = Numb(self.0);
    match numb.get_typ() {
      TY_SYM => match numb.get_sym() as Tag {
        // casts
        TY_U24 => "[u24]".to_string(),
        TY_I24 => "[i24]".to_string(),
        TY_F24 => "[f24]".to_string(),
        // operations
        OP_ADD => "[+]".to_string(),
        OP_SUB => "[-]".to_string(),
        FP_SUB => "[:-]".to_string(),
        OP_MUL => "[*]".to_string(),
        OP_DIV => "[/]".to_string(),
        FP_DIV => "[:/]".to_string(),
        OP_REM => "[%]".to_string(),
        FP_REM => "[:%]".to_string(),
        OP_EQ => "[=]".to_string(),
        OP_NEQ => "[!]".to_string(),
        OP_LT => "[<]".to_string(),
        OP_GT => "[>]".to_string(),
        OP_AND => "[&]".to_string(),
        OP_OR => "[|]".to_string(),
        OP_XOR => "[^]".to_string(),
        OP_SHL => "[<<]".to_string(),
        FP_SHL => "[:<<]".to_string(),
        OP_SHR => "[>>]".to_string(),
        FP_SHR => "[:>>]".to_string(),
        _ => "[?]".to_string(),
      },
      TY_U24 => {
        let val = numb.get_u24();
        format!("{}", val)
      }
      TY_I24 => {
        let val = numb.get_i24();
        format!("{:+}", val)
      }
      TY_F24 => {
        let val = numb.get_f24();
        if val.is_infinite() {
          if val.is_sign_positive() {
            "+inf".to_string()
          } else {
            "-inf".to_string()
          }
        } else if val.is_nan() {
          "+NaN".to_string()
        } else {
          format!("{:?}", val)
        }
      }
      _ => {
        let typ = numb.get_typ();
        let val = numb.get_u24();
        format!(
          "[{}0x{:07X}]",
          match typ {
            OP_ADD => "+",
            OP_SUB => "-",
            FP_SUB => ":-",
            OP_MUL => "*",
            OP_DIV => "/",
            FP_DIV => ":/",
            OP_REM => "%",
            FP_REM => ":%",
            OP_EQ => "=",
            OP_NEQ => "!",
            OP_LT => "<",
            OP_GT => ">",
            OP_AND => "&",
            OP_OR => "|",
            OP_XOR => "^",
            OP_SHL => "<<",
            FP_SHL => ":<<",
            OP_SHR => ">>",
            FP_SHR => ":>>",
            _ => "?",
          },
          val
        )
      }
    }
  }
}

impl Tree {
  pub fn show(&self) -> String {
    match self {
      Tree::Var { nam } => format!("+{}", nam),
      Tree::Sub { nam } => format!("-{}", nam),
      Tree::Lam { fst, snd } => format!("+({} {})", fst.show(), snd.show()),
      Tree::App { fst, snd } => format!("-({} {})", fst.show(), snd.show()),
      Tree::Sup { fst, snd } => format!("+{{{} {}}}", fst.show(), snd.show()),
      Tree::Dup { fst, snd } => format!("-{{{} {}}}", fst.show(), snd.show()),
      Tree::Era => "+*".to_string(),
      Tree::Del => "-*".to_string(),
      Tree::Ref { nam } => format!("@{}", nam),
      //Tree::Num { val } => val.show().to_string(),
      //Tree::Opr { fst, snd } => format!("$({} {})", fst.show(), snd.show()),
      //Tree::Swi { fst, snd } => format!("?({} {})", fst.show(), snd.show()),
    }
  }
}

impl Net {
  pub fn show(&self) -> String {
    let mut s = self.root.show();
    for (par, fst, snd) in &self.rbag {
      s.push_str(" &");
      s.push_str(if *par { "!" } else { " " });
      s.push_str(&fst.show());
      s.push_str(" ~ ");
      s.push_str(&snd.show());
    }
    s
  }
}

impl Book {
  pub fn show(&self) -> String {
    let mut s = String::new();
    for (name, net) in &self.defs {
      s.push('@');
      s.push_str(name);
      s.push_str(" = ");
      s.push_str(&net.show());
      s.push('\n');
    }
    s
  }
}

// Number conversions
impl Numb {
  // SYM: a symbolic operator

  pub fn new_sym(val: Tag) -> Self {
    Numb((val as Val) << 5 | (TY_SYM as Val))
  }

  pub fn get_sym(&self) -> Tag {
    (self.0 >> 5) as Tag
  }

  // U24: unsigned 24-bit integer

  pub fn new_u24(val: u32) -> Self {
    Numb((val << 5) as Val | (TY_U24 as Val))
  }

  pub fn get_u24(&self) -> u32 {
    self.0 >> 5
  }

  // I24: signed 24-bit integer

  pub fn new_i24(val: i32) -> Self {
    Numb(((val as u32) << 5) as Val | (TY_I24 as Val))
  }

  pub fn get_i24(&self) -> i32 {
    (self.0 as i32) << 3 >> 8
  }

  // F24: 24-bit float

  pub fn new_f24(val: f32) -> Self {
    let bits = val.to_bits();
    let mut shifted_bits = bits >> 8;
    let lost_bits = bits & 0xFF;
    // round ties to even
    shifted_bits += u32::from(!val.is_nan()) & ((lost_bits - ((lost_bits >> 7) & !shifted_bits)) >> 7);
    // ensure NaNs don't become infinities
    shifted_bits |= u32::from(val.is_nan());
    Numb((shifted_bits << 5) as Val | (TY_F24 as Val))
  }

  pub fn get_f24(&self) -> f32 {
    f32::from_bits((self.0 << 3) & 0xFFFFFF00)
  }

  // Gets the numeric type.
  pub fn get_typ(&self) -> Tag {
    (self.0 & 0x1F) as Tag
  }

  pub fn is_num(&self) -> bool {
    self.get_typ() >= TY_U24 && self.get_typ() <= TY_F24
  }

  pub fn is_cast(&self) -> bool {
    self.get_typ() == TY_SYM && self.get_sym() >= TY_U24 && self.get_sym() <= TY_F24
  }

  // Partial application.
  pub fn partial(a: Self, b: Self) -> Self {
    Numb((b.0 & !0x1F) | a.get_sym() as u32)
  }

  // Cast a number to another type.
  // The semantics are meant to spiritually resemble rust's numeric casts:
  // - i24 <-> u24: is just reinterpretation of bits
  // - f24  -> i24,
  //   f24  -> u24: casts to the "closest" integer representing this float,
  //                saturating if out of range and 0 if NaN
  // - i24  -> f24,
  //   u24  -> f24: casts to the "closest" float representing this integer.
  pub fn cast(a: Self, b: Self) -> Self {
    match (a.get_sym(), b.get_typ()) {
      (TY_U24, TY_U24) => b,
      (TY_U24, TY_I24) => Self::new_u24(b.get_i24() as u32),
      (TY_U24, TY_F24) => Self::new_u24((b.get_f24() as u32).clamp(U24_MIN, U24_MAX)),

      (TY_I24, TY_U24) => Self::new_i24(b.get_u24() as i32),
      (TY_I24, TY_I24) => b,
      (TY_I24, TY_F24) => Self::new_i24((b.get_f24() as i32).clamp(I24_MIN, I24_MAX)),

      (TY_F24, TY_U24) => Self::new_f24(b.get_u24() as f32),
      (TY_F24, TY_I24) => Self::new_f24(b.get_i24() as f32),
      (TY_F24, TY_F24) => b,
      // invalid cast
      (_, _) => Self::new_u24(0),
    }
  }

  pub fn operate(a: Self, b: Self) -> Self {
    //println!("operate {} {}", crate::ast::Numb(a.0).show(), crate::ast::Numb(b.0).show());
    let at = a.get_typ();
    let bt = b.get_typ();
    if at == TY_SYM && bt == TY_SYM {
      return Numb::new_u24(0);
    }
    if a.is_cast() && b.is_num() {
      return Numb::cast(a, b);
    }
    if b.is_cast() && a.is_num() {
      return Numb::cast(b, a);
    }
    if at == TY_SYM && bt != TY_SYM {
      return Numb::partial(a, b);
    }
    if at != TY_SYM && bt == TY_SYM {
      return Numb::partial(b, a);
    }
    if at >= OP_ADD && bt >= OP_ADD {
      return Numb::new_u24(0);
    }
    if at < OP_ADD && bt < OP_ADD {
      return Numb::new_u24(0);
    }
    let (op, a, ty, b) = if at >= OP_ADD { (at, a, bt, b) } else { (bt, b, at, a) };
    match ty {
      TY_U24 => {
        let av = a.get_u24();
        let bv = b.get_u24();
        match op {
          OP_ADD => Numb::new_u24(av.wrapping_add(bv)),
          OP_SUB => Numb::new_u24(av.wrapping_sub(bv)),
          FP_SUB => Numb::new_u24(bv.wrapping_sub(av)),
          OP_MUL => Numb::new_u24(av.wrapping_mul(bv)),
          OP_DIV => Numb::new_u24(av.wrapping_div(bv)),
          FP_DIV => Numb::new_u24(bv.wrapping_div(av)),
          OP_REM => Numb::new_u24(av.wrapping_rem(bv)),
          FP_REM => Numb::new_u24(bv.wrapping_rem(av)),
          OP_EQ => Numb::new_u24((av == bv) as u32),
          OP_NEQ => Numb::new_u24((av != bv) as u32),
          OP_LT => Numb::new_u24((av < bv) as u32),
          OP_GT => Numb::new_u24((av > bv) as u32),
          OP_AND => Numb::new_u24(av & bv),
          OP_OR => Numb::new_u24(av | bv),
          OP_XOR => Numb::new_u24(av ^ bv),
          OP_SHL => Numb::new_u24(av << (bv & 31)),
          OP_SHR => Numb::new_u24(av >> (bv & 31)),
          FP_SHL => Numb::new_u24(bv << (av & 31)),
          FP_SHR => Numb::new_u24(bv >> (av & 31)),
          _ => unreachable!(),
        }
      }
      TY_I24 => {
        let av = a.get_i24();
        let bv = b.get_i24();
        match op {
          OP_ADD => Numb::new_i24(av.wrapping_add(bv)),
          OP_SUB => Numb::new_i24(av.wrapping_sub(bv)),
          FP_SUB => Numb::new_i24(bv.wrapping_sub(av)),
          OP_MUL => Numb::new_i24(av.wrapping_mul(bv)),
          OP_DIV => Numb::new_i24(av.wrapping_div(bv)),
          FP_DIV => Numb::new_i24(bv.wrapping_div(av)),
          OP_REM => Numb::new_i24(av.wrapping_rem(bv)),
          FP_REM => Numb::new_i24(bv.wrapping_rem(av)),
          OP_EQ => Numb::new_u24((av == bv) as u32),
          OP_NEQ => Numb::new_u24((av != bv) as u32),
          OP_LT => Numb::new_u24((av < bv) as u32),
          OP_GT => Numb::new_u24((av > bv) as u32),
          OP_AND => Numb::new_i24(av & bv),
          OP_OR => Numb::new_i24(av | bv),
          OP_XOR => Numb::new_i24(av ^ bv),
          _ => unreachable!(),
        }
      }
      TY_F24 => {
        let av = a.get_f24();
        let bv = b.get_f24();
        match op {
          OP_ADD => Numb::new_f24(av + bv),
          OP_SUB => Numb::new_f24(av - bv),
          FP_SUB => Numb::new_f24(bv - av),
          OP_MUL => Numb::new_f24(av * bv),
          OP_DIV => Numb::new_f24(av / bv),
          FP_DIV => Numb::new_f24(bv / av),
          OP_REM => Numb::new_f24(av % bv),
          FP_REM => Numb::new_f24(bv % av),
          OP_EQ => Numb::new_u24((av == bv) as u32),
          OP_NEQ => Numb::new_u24((av != bv) as u32),
          OP_LT => Numb::new_u24((av < bv) as u32),
          OP_GT => Numb::new_u24((av > bv) as u32),
          OP_AND => Numb::new_f24(av.atan2(bv)),
          OP_OR => Numb::new_f24(bv.log(av)),
          OP_XOR => Numb::new_f24(av.powf(bv)),
          OP_SHL => Numb::new_f24((av + bv).sin()),
          OP_SHR => Numb::new_f24((av + bv).tan()),
          _ => unreachable!(),
        }
      }
      _ => Numb::new_u24(0),
    }
  }
}

const U24_MAX: u32 = (1 << 24) - 1;
const U24_MIN: u32 = 0;
const I24_MAX: i32 = (1 << 23) - 1;
const I24_MIN: i32 = (-1) << 23;

pub const TY_SYM: Tag = 0x00;
pub const TY_U24: Tag = 0x01;
pub const TY_I24: Tag = 0x02;
pub const TY_F24: Tag = 0x03;
pub const OP_ADD: Tag = 0x04;
pub const OP_SUB: Tag = 0x05;
pub const FP_SUB: Tag = 0x06;
pub const OP_MUL: Tag = 0x07;
pub const OP_DIV: Tag = 0x08;
pub const FP_DIV: Tag = 0x09;
pub const OP_REM: Tag = 0x0A;
pub const FP_REM: Tag = 0x0B;
pub const OP_EQ: Tag = 0x0C;
pub const OP_NEQ: Tag = 0x0D;
pub const OP_LT: Tag = 0x0E;
pub const OP_GT: Tag = 0x0F;
pub const OP_AND: Tag = 0x10;
pub const OP_OR: Tag = 0x11;
pub const OP_XOR: Tag = 0x12;
pub const OP_SHL: Tag = 0x13;
pub const FP_SHL: Tag = 0x14;
pub const OP_SHR: Tag = 0x15;
pub const FP_SHR: Tag = 0x16;
