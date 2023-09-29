use super::{DefId, Name};
use bimap::{BiHashMap, Overwritten};
use itertools::Itertools;
use std::fmt;

#[cfg(feature = "nums")]
use hvm_core::{opx_to_string, OP};

#[derive(Debug, Clone, Default)]
pub struct DefNames {
  map: BiHashMap<DefId, Name>,
  id_count: DefId,
}

#[derive(Debug, Clone, Default)]
pub struct DefinitionBook {
  pub def_names: DefNames,
  pub defs: Vec<Definition>,
}

#[derive(Debug, Clone)]
pub struct Definition {
  pub def_id: DefId,
  pub rules: Vec<Rule>,
}

#[derive(Debug, Clone)]
pub struct Rule {
  pub def_id: DefId,
  pub pats: Vec<Pattern>,
  pub body: Term,
}

#[derive(Debug, Clone)]
pub enum Pattern {
  Ctr(Name, Vec<Pattern>),
  Var(Option<Name>),
  #[cfg(feature = "nums")]
  U32(u32),
  #[cfg(feature = "nums")]
  I32(i32),
}

#[derive(Debug, Clone)]
pub enum Term {
  Lam {
    nam: Option<Name>,
    bod: Box<Term>,
  },
  Var {
    nam: Name,
  },
  /// Like a scopeless lambda, where the variable can occur outside the body
  Chn {
    nam: Name,
    bod: Box<Term>,
  },
  /// The use of a Channel variable.
  Lnk {
    nam: Name,
  },
  Let {
    nam: Name,
    val: Box<Term>,
    nxt: Box<Term>,
  },
  Ref {
    def_id: DefId,
  },
  App {
    fun: Box<Term>,
    arg: Box<Term>,
  },
  Dup {
    fst: Option<Name>,
    snd: Option<Name>,
    val: Box<Term>,
    nxt: Box<Term>,
  },
  Sup {
    fst: Box<Term>,
    snd: Box<Term>,
  },
  Era,
  #[cfg(feature = "nums")]
  U32 {
    val: u32,
  },
  #[cfg(feature = "nums")]
  I32 {
    val: i32,
  },
  #[cfg(feature = "nums")]
  /// A numeric operation between built-in numbers.
  Opx {
    op: OP,
    fst: Box<Term>,
    snd: Box<Term>,
  },
}

impl DefinitionBook {
  pub fn new() -> Self {
    Default::default()
  }
}

impl DefNames {
  pub fn new() -> Self {
    Default::default()
  }

  pub fn name(&self, def_id: &DefId) -> Option<&Name> {
    self.map.get_by_left(def_id)
  }

  pub fn def_id(&self, name: &Name) -> Option<DefId> {
    self.map.get_by_right(name).copied()
  }

  pub fn contains_name(&self, name: &Name) -> bool {
    self.map.contains_right(name)
  }

  pub fn contains_def_id(&self, def_id: &DefId) -> bool {
    self.map.contains_left(def_id)
  }

  pub fn insert(&mut self, name: Name) -> DefId {
    let def_id = self.id_count;
    self.id_count.0 += 1;
    match self.map.insert(def_id, name) {
      Overwritten::Neither => def_id,
      _ => todo!("Overwritting name-id pairs not supported"),
    }
  }
}

impl Term {
  pub fn to_string(&self, def_names: &DefNames) -> String {
    match self {
      Term::Lam { nam, bod } => {
        format!("λ{} {}", nam.clone().unwrap_or(Name::new("*")), bod.to_string(def_names))
      }
      Term::Var { nam } => format!("{nam}"),
      Term::Chn { nam, bod } => format!("λ${} {}", nam, bod.to_string(def_names)),
      Term::Lnk { nam } => format!("${nam}"),
      Term::Let { nam, val, nxt } => {
        format!("let {} = {}; {}", nam, val.to_string(def_names), nxt.to_string(def_names))
      }
      Term::Ref { def_id } => format!("{}", def_names.name(def_id).unwrap()),
      Term::App { fun, arg } => format!("({} {})", fun.to_string(def_names), arg.to_string(def_names)),
      Term::Dup { fst, snd, val, nxt } => format!(
        "dup {} {} = {}; {}",
        fst.as_ref().map(|x| x.as_str()).unwrap_or("*"),
        snd.as_ref().map(|x| x.as_str()).unwrap_or("*"),
        val.to_string(def_names),
        nxt.to_string(def_names)
      ),
      Term::Sup { fst, snd } => format!("{{{} {}}}", fst.to_string(def_names), snd.to_string(def_names)),
      Term::Era => "*".to_string(),
      #[cfg(feature = "nums")]
      Term::U32 { val } => format!("{val}"),
      #[cfg(feature = "nums")]
      Term::I32 { val } => format!("{val:+}"),
      #[cfg(feature = "nums")]
      Term::Opx { op, fst, snd } => {
        format!("({} {} {})", opx_to_string(op), fst.to_string(def_names), snd.to_string(def_names))
      }
    }
  }

  /// Make a call term by folding args around a called function term with applications.
  pub fn call(called: Term, args: impl IntoIterator<Item = Term>) -> Self {
    args.into_iter().fold(called, |acc, arg| Term::App { fun: Box::new(acc), arg: Box::new(arg) })
  }

  /// Substitute the occurences of a variable in a term with the given term.
  pub fn subst(&mut self, from: &Name, to: &Term) {
    match self {
      Term::Lam { nam: None, bod } => bod.subst(from, to),
      Term::Lam { nam: Some(nam), bod } if nam == from => (),
      Term::Lam { nam: Some(nam), bod } => bod.subst(from, to),
      Term::Var { nam } if nam == from => *self = to.clone(),
      Term::Var { nam } => (),
      // Only substitute scoped variables.
      Term::Chn { nam, bod } => bod.subst(from, to),
      Term::Lnk { nam } => (),
      Term::Let { nam, val, nxt } => {
        val.subst(from, to);
        if nam != from {
          nxt.subst(from, to);
        }
      }
      Term::Ref { def_id } => (),
      Term::App { fun, arg } => {
        fun.subst(from, to);
        arg.subst(from, to);
      }
      Term::Dup { fst, snd, val, nxt } => {
        val.subst(from, to);
        if fst.as_ref().map_or(true, |fst| fst == from) && fst.as_ref().map_or(true, |snd| snd == from) {
          nxt.subst(from, to);
        }
      }
      Term::Sup { fst, snd } => {
        fst.subst(from, to);
        snd.subst(from, to);
      }
      Term::Era => (),
    }
  }
}

impl Rule {
  pub fn to_string(&self, def_names: &DefNames) -> String {
    let Rule { def_id, pats, body } = self;
    format!(
      "({}{}) = {}",
      def_names.name(def_id).unwrap(),
      pats.iter().map(|x| format!(" {x}")).join(""),
      body.to_string(def_names)
    )
  }
}

impl Definition {
  pub fn to_string(&self, def_names: &DefNames) -> String {
    self.rules.iter().map(|x| x.to_string(def_names)).join("\n")
  }
}

impl From<&Pattern> for Term {
  fn from(value: &Pattern) -> Self {
    match value {
      Pattern::Ctr(nam, args) => Term::call(Term::Var { nam: nam.clone() }, args.iter().map(Term::from)),
      Pattern::Var(nam) => Term::Var { nam: Name::new(nam.as_ref().map(|x| x.as_str()).unwrap_or("_")) },
      #[cfg(feature = "nums")]
      Pattern::U32(num) => Term::U32 { num },
      #[cfg(feature = "nums")]
      Pattern::I32(num) => Term::I32 { num },
    }
  }
}

impl fmt::Display for DefinitionBook {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      write!(f, "{}", self.defs.iter().map(|x| x.to_string(&self.def_names)).join("\n\n"))
    }
}

impl fmt::Display for Pattern {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Pattern::Ctr(name, pats) => write!(f, "({}{})", name, pats.iter().map(|p| format!(" {p}")).join("")),
      Pattern::Var(nam) => write!(f, "{}", nam.as_ref().map(|x| x.as_str()).unwrap_or("*")),
      #[cfg(feature = "nums")]
      Pattern::U32(num) => write!(f, "{num}"),
      #[cfg(feature = "nums")]
      Pattern::I32(num) => write!(f, "{num:+}"),
    }
  }
}
