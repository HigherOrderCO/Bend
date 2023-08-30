use std::{collections::HashMap, fmt};

use super::DefId;
pub struct Book {
  pub defs: HashMap<DefId, LNet>,
}

// TODO: Import this module directly from hvm2

#[derive(Debug)]
pub struct LNet {
  root: LTree,
  acts: LActs,
}

#[derive(Debug)]
pub enum LTree {
  Era,
  Nod { tag: u8, lft: Box<LTree>, rgt: Box<LTree> },
  Var { nam: String },
  Num { val: u32 },
  Ref { nam: u32 },
}

type LActs = Vec<(LTree, LTree)>;

pub fn name_to_letters(name: &str) -> Vec<u8> {
  let mut letters = Vec::new();
  for c in name.chars() {
    letters.push(match c {
      '.' => 0,
      '0' ..= '9' => c as u8 - b'0' + 1,
      'A' ..= 'Z' => c as u8 - b'A' + 11,
      'a' ..= 'z' => c as u8 - b'a' + 37,
      '_' => 63,
      _ => panic!("Invalid character in name"),
    });
  }
  letters
}

pub fn letters_to_name(letters: Vec<u8>) -> String {
  let mut name = String::new();
  for letter in letters {
    name.push(match letter {
      0 => '.',
      1 ..= 10 => (letter - 1 + b'0') as char,
      11 ..= 36 => (letter - 11 + b'A') as char,
      37 ..= 62 => (letter - 37 + b'a') as char,
      63 => '_',
      _ => panic!("Invalid letter in name"),
    });
  }
  name
}

pub fn u32_to_letters(num: u32) -> Vec<u8> {
  let mut letters = Vec::new();
  let mut num = num;
  while num > 0 {
    letters.push((num % 64) as u8);
    num /= 64;
  }
  letters.reverse();
  letters
}

pub fn letters_to_u32(letters: Vec<u8>) -> u32 {
  let mut num = 0;
  for letter in letters {
    num = num * 64 + letter as u32;
  }
  num
}

pub fn name_to_u32(name: &str) -> u32 {
  letters_to_u32(name_to_letters(name))
}

pub fn u32_to_name(num: u32) -> String {
  letters_to_name(u32_to_letters(num))
}

impl fmt::Display for LNet {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    writeln!(f, "  $ {}", self.root)?;
    for (a, b) in &self.acts {
      writeln!(f, "  & {}", a)?;
      writeln!(f, "  ~ {}", b)?;
    }
    Ok(())
  }
}

impl fmt::Display for LTree {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      LTree::Era => write!(f, "*"),
      LTree::Nod { tag, lft, rgt } => write!(f, "({} {} {})", tag - CON, lft, rgt),
      LTree::Var { nam } => write!(f, "{nam}"),
      LTree::Num { val } => write!(f, "{val}"),
      LTree::Ref { nam } => write!(f, "@{}", letters_to_name(u32_to_letters(*nam))),
    }
  }
}

impl fmt::Display for Book {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for (id, net) in &self.defs {
      writeln!(f, "{} =", u32_to_name(**id))?;
      writeln!(f, "{}\n", net)?;
    }
    Ok(())
  }
}

pub type Tag = u8;
pub type Val = u32;

pub const NIL: Tag = 0x0; // empty node
pub const REF: Tag = 0x1; // reference to a definition (closed net)
pub const NUM: Tag = 0x2; // unboxed number
pub const ERA: Tag = 0x3; // unboxed eraser
pub const VRR: Tag = 0x4; // variable pointing to root
pub const VR1: Tag = 0x5; // variable pointing to aux1 port of node
pub const VR2: Tag = 0x6; // variable pointing to aux2 port of node
pub const RDT: Tag = 0x7; // redirection to root
pub const RD1: Tag = 0x8; // redirection to aux1 port of node
pub const RD2: Tag = 0x9; // redirection to aux2 port of node
pub const CON: Tag = 0xA; // points to main port of con node
pub const DUP: Tag = 0xB; // points to main port of dup node; higher labels also dups
