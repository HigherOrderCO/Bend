use hvmc::run::Val;
use indexmap::IndexMap;
use itertools::Itertools;
use shrinkwraprs::Shrinkwrap;
use std::{
  collections::{BTreeMap, HashMap},
  fmt,
};

pub mod check;
pub mod load_book;
pub mod net_to_term;
pub mod parser;
pub mod term_to_net;
pub mod transform;

pub use net_to_term::net_to_term_linear;
pub use term_to_net::{book_to_nets, term_to_compat_net};

/// The representation of a program.
#[derive(Debug, Clone, Default)]
pub struct Book {
  /// Mapping of definition names to ids.
  pub def_names: DefNames,

  /// The function definitions.
  pub defs: BTreeMap<DefId, Definition>,

  /// The algebraic datatypes defined by the program
  pub adts: BTreeMap<Name, Adt>,

  /// To which type does each constructor belong to.
  pub ctrs: HashMap<Name, Name>,
}

#[derive(Debug, Clone, Default)]
pub struct DefNames {
  id_to_name: HashMap<DefId, Name>,
  name_to_id: HashMap<Name, DefId>,
  id_count: DefId,
}

/// A pattern matching function definition.
#[derive(Debug, Clone)]
pub struct Definition {
  pub def_id: DefId,
  pub rules: Vec<Rule>,
}

/// A pattern matching rule of a definition.
#[derive(Debug, Clone)]
pub struct Rule {
  pub pats: Vec<Pattern>,
  pub body: Term,
}

#[derive(Debug, Clone)]
pub enum MatchNum {
  Zero,
  Succ(Option<Name>),
}

#[derive(Debug, Clone, Default)]
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
    pat: Pattern,
    val: Box<Term>,
    nxt: Box<Term>,
  },
  App {
    fun: Box<Term>,
    arg: Box<Term>,
  },
  Tup {
    fst: Box<Term>,
    snd: Box<Term>,
  },
  Dup {
    tag: Option<Name>,
    fst: Option<Name>,
    snd: Option<Name>,
    val: Box<Term>,
    nxt: Box<Term>,
  },
  Sup {
    tag: Name,
    fst: Box<Term>,
    snd: Box<Term>,
  },
  Num {
    val: u64,
  },
  /// A numeric operation between built-in numbers.
  Opx {
    op: Op,
    fst: Box<Term>,
    snd: Box<Term>,
  },
  Match {
    scrutinee: Box<Term>,
    arms: Vec<(Pattern, Term)>,
  },
  Ref {
    def_id: DefId,
  },
  #[default]
  Era,
}

#[derive(Debug, Clone)]
pub enum Pattern {
  Var(Name),
  Ctr(Name, Vec<Pattern>),
  Num(MatchNum),
  Tup(Option<Name>, Option<Name>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
  ADD,
  SUB,
  MUL,
  DIV,
  MOD,
  EQ,
  NE,
  LT,
  GT,
  LTE,
  GTE,
  AND,
  OR,
  XOR,
  LSH,
  RSH,
  NOT,
}

/// A user defined  datatype
#[derive(Debug, Clone, Default)]
pub struct Adt {
  pub ctrs: IndexMap<Name, Vec<Name>>,
}

#[derive(Debug, PartialEq, Eq, Clone, Shrinkwrap, Hash, PartialOrd, Ord)]
pub struct Name(pub String);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Shrinkwrap, Hash, PartialOrd, Ord, Default)]
pub struct DefId(pub Val);

pub fn var_id_to_name(mut var_id: Val) -> Name {
  let mut name = String::new();
  loop {
    let c = (var_id % 26) as u8 + b'a';
    name.push(c as char);
    var_id /= 26;
    if var_id == 0 {
      break;
    }
  }
  Name(name)
}

impl Name {
  pub fn new(value: &str) -> Self {
    Name(value.to_string())
  }
}

impl DefId {
  // TODO: We use this workaround because hvm-core's val_to_name function doesn't work with value 0
  pub fn to_internal(self) -> Val {
    *self + 1
  }

  pub fn from_internal(val: Val) -> Self {
    Self(val - 1)
  }
}

impl Book {
  pub fn new() -> Self {
    Default::default()
  }

  pub fn insert_def(&mut self, name: Name, rules: Vec<Rule>) -> DefId {
    let def_id = self.def_names.insert(name);
    let def = Definition { def_id, rules };
    self.defs.insert(def_id, def);
    def_id
  }

  pub fn remove_def(&mut self, def_id: DefId) -> Option<(Name, Definition)> {
    let def = self.defs.remove(&def_id);
    let name = self.def_names.remove(def_id);
    name.zip(def)
  }
}

impl DefNames {
  pub const ENTRY_POINT: &'static str = "main";
  pub const HVM1_ENTRY_POINT: &'static str = "Main";

  pub fn new() -> Self {
    Default::default()
  }

  pub fn name(&self, def_id: &DefId) -> Option<&Name> {
    self.id_to_name.get(def_id)
  }

  pub fn def_id(&self, name: &Name) -> Option<DefId> {
    self.name_to_id.get(name).copied()
  }

  pub fn contains_name(&self, name: &Name) -> bool {
    self.name_to_id.contains_key(name)
  }

  pub fn contains_def_id(&self, def_id: &DefId) -> bool {
    self.id_to_name.contains_key(def_id)
  }

  pub fn insert(&mut self, name: Name) -> DefId {
    let def_id = self.id_count;
    self.id_count.0 += 1;
    self.id_to_name.insert(def_id, name.clone());
    self.name_to_id.insert(name, def_id);
    def_id
  }

  pub fn remove(&mut self, def_id: DefId) -> Option<Name> {
    let nam = self.id_to_name.remove(&def_id);
    if let Some(nam) = &nam {
      self.name_to_id.remove(nam);
    }
    nam
  }

  pub fn names(&self) -> impl Iterator<Item = &Name> {
    self.name_to_id.keys()
  }

  pub fn def_ids(&self) -> impl Iterator<Item = &DefId> {
    self.id_to_name.keys()
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
      Term::Let { pat, val, nxt } => {
        format!("let {} = {}; {}", pat, val.to_string(def_names), nxt.to_string(def_names))
      }
      Term::Ref { def_id } => format!("{}", def_names.name(def_id).unwrap()),
      Term::App { fun, arg } => format!("({} {})", fun.to_string(def_names), arg.to_string(def_names)),
      Term::Match { scrutinee, arms } => {
        let arms =
          arms.iter().map(|(pat, term)| format!("{}: {}", pat, term.to_string(def_names))).join("; ");

        format!("match {} {{ {} }}", scrutinee.to_string(def_names), arms,)
      }
      Term::Dup { tag: _, fst, snd, val, nxt } => format!(
        "dup {} {} = {}; {}",
        fst.as_ref().map(|x| x.as_str()).unwrap_or("*"),
        snd.as_ref().map(|x| x.as_str()).unwrap_or("*"),
        val.to_string(def_names),
        nxt.to_string(def_names)
      ),
      Term::Sup { tag, fst, snd } => {
        format!("{{#{} {} {}}}", tag, fst.to_string(def_names), snd.to_string(def_names))
      }
      Term::Era => "*".to_string(),
      Term::Num { val } => format!("{val}"),
      Term::Opx { op, fst, snd } => {
        format!("({} {} {})", op, fst.to_string(def_names), snd.to_string(def_names))
      }
      Term::Tup { fst, snd } => format!("({}, {})", fst.to_string(def_names), snd.to_string(def_names)),
    }
  }

  /// Make a call term by folding args around a called function term with applications.
  pub fn call(called: Term, args: impl IntoIterator<Item = Term>) -> Self {
    args.into_iter().fold(called, |acc, arg| Term::App { fun: Box::new(acc), arg: Box::new(arg) })
  }

  /// Substitute the occurences of a variable in a term with the given term.
  pub fn subst(&mut self, from: &Name, to: &Term) {
    match self {
      Term::Lam { nam: Some(nam), .. } if nam == from => (),
      Term::Lam { bod, .. } => bod.subst(from, to),
      Term::Var { nam } if nam == from => *self = to.clone(),
      Term::Var { .. } => (),
      // Only substitute scoped variables.
      Term::Chn { bod, .. } => bod.subst(from, to),
      Term::Lnk { .. } => (),
      Term::Let { pat: Pattern::Var(nam), val, nxt } => {
        val.subst(from, to);
        if nam != from {
          nxt.subst(from, to);
        }
      }
      Term::Let { pat: Pattern::Tup(fst, snd), val, nxt } => {
        val.subst(from, to);
        if fst.as_ref().map_or(true, |fst| fst != from) && snd.as_ref().map_or(true, |snd| snd != from) {
          nxt.subst(from, to);
        }
      }
      Term::Let { .. } => todo!(),
      Term::Dup { tag: _, fst, snd, val, nxt } => {
        val.subst(from, to);
        if fst.as_ref().map_or(true, |fst| fst != from) && snd.as_ref().map_or(true, |snd| snd != from) {
          nxt.subst(from, to);
        }
      }
      Term::Match { scrutinee, arms } => {
        scrutinee.subst(from, to);

        for (rule, term) in arms {
          let can_subst;

          if let Pattern::Num(MatchNum::Succ(Some(nam))) = rule {
            can_subst = nam != from
          } else {
            can_subst = true
          };

          if can_subst {
            term.subst(from, to);
          }
        }
      }
      Term::App { fun: fst, arg: snd }
      | Term::Sup { fst, snd, .. }
      | Term::Tup { fst, snd }
      | Term::Opx { fst, snd, .. } => {
        fst.subst(from, to);
        snd.subst(from, to);
      }
      Term::Ref { .. } | Term::Num { .. } | Term::Era => (),
    }
  }

  /// Collects all the free variables that a term has
  /// and the number of times each var is used
  pub fn free_vars(&self) -> IndexMap<Name, u64> {
    fn go(term: &Term, free_vars: &mut IndexMap<Name, u64>) {
      match term {
        Term::Lam { nam: Some(nam), bod } => {
          let mut new_scope = IndexMap::new();
          go(bod, &mut new_scope);
          new_scope.remove(nam);

          free_vars.extend(new_scope);
        }
        Term::Lam { nam: None, bod } => go(bod, free_vars),
        Term::Var { nam } => *free_vars.entry(nam.clone()).or_default() += 1,
        Term::Chn { bod, .. } => go(bod, free_vars),
        Term::Lnk { .. } => {}
        Term::Let { pat: Pattern::Var(nam), val, nxt } => {
          go(val, free_vars);

          let mut new_scope = IndexMap::new();
          go(nxt, &mut new_scope);

          new_scope.remove(nam);

          free_vars.extend(new_scope);
        }
        Term::Let { pat: Pattern::Tup(fst, snd), val, nxt } | Term::Dup { fst, snd, val, nxt, .. } => {
          go(val, free_vars);

          let mut new_scope = IndexMap::new();
          go(nxt, &mut new_scope);

          fst.as_ref().map(|fst| new_scope.remove(fst));
          snd.as_ref().map(|snd| new_scope.remove(snd));

          free_vars.extend(new_scope);
        }
        Term::Let { .. } => todo!(),
        Term::App { fun: fst, arg: snd }
        | Term::Tup { fst, snd }
        | Term::Sup { fst, snd, .. }
        | Term::Opx { op: _, fst, snd } => {
          go(fst, free_vars);
          go(snd, free_vars);
        }
        Term::Match { scrutinee, arms } => {
          go(scrutinee, free_vars);

          for (rule, term) in arms {
            let mut new_scope = IndexMap::new();
            go(term, &mut new_scope);

            if let Pattern::Num(MatchNum::Succ(Some(nam))) = rule {
              new_scope.remove(nam);
            }

            free_vars.extend(new_scope);
          }
        }
        Term::Ref { .. } | Term::Num { .. } | Term::Era => {}
      }
    }

    let mut free_vars = IndexMap::new();
    go(self, &mut free_vars);
    free_vars
  }

  /// Creates a new [`Term::Match`] from the given terms.
  /// If `scrutinee` is not a `Term::Var`, creates a let binding containing the match in its body
  pub fn new_native_match(
    scrutinee: Self,
    zero_term: Self,
    mut succ_label: Option<Name>,
    mut succ_term: Self,
  ) -> Self {
    let zero = (Pattern::Num(MatchNum::Zero), zero_term);

    if let Term::Var { nam } = &scrutinee {
      if let Some(label) = &succ_label {
        let new_label = Name(format!("{}-1", nam));
        succ_term.subst(label, &Term::Var { nam: new_label.clone() });
        succ_label = Some(new_label);
      }

      let succ = (Pattern::Num(MatchNum::Succ(succ_label)), succ_term);
      Term::Match { scrutinee: Box::new(scrutinee), arms: vec![zero, succ] }
    } else {
      let match_bind = succ_label.clone().unwrap_or_else(|| Name::new("*"));

      if let Some(label) = &succ_label {
        let new_label = Name(format!("{}-1", label));
        succ_term.subst(label, &Term::Var { nam: new_label.clone() });
        succ_label = Some(new_label);
      }

      let succ = (Pattern::Num(MatchNum::Succ(succ_label)), succ_term);

      Term::Let {
        pat: Pattern::Var(match_bind.clone()),
        val: Box::new(scrutinee),
        nxt: Box::new(Term::Match {
          scrutinee: Box::new(Term::Var { nam: match_bind }),
          arms: vec![zero, succ],
        }),
      }
    }
  }
}

/// Returns the lambda representation of native number match arms
pub fn native_match(arms: Vec<(Pattern, Term)>) -> (Term, Term) {
  use MatchNum::*;

  match &arms[..] {
    [(Pattern::Num(Zero), zero), (Pattern::Num(Succ(nam)), succ)] => {
      let zero = zero.clone();
      let succ = Term::Lam { nam: nam.clone(), bod: Box::new(succ.clone()) };
      (zero, succ)
    }
    [(Pattern::Num(Zero), zero), (Pattern::Num(Zero), succ)] => (zero.clone(), succ.clone()),
    _ => unreachable!(),
  }
}

impl fmt::Display for Pattern {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Pattern::Var(nam) => write!(f, "{nam}"),
      Pattern::Ctr(nam, pats) => write!(f, "({}{})", nam, pats.iter().map(|p| format!(" {p}")).join("")),
      Pattern::Num(num) => write!(f, "{num}"),
      Pattern::Tup(fst, snd) => write!(
        f,
        "({}, {})",
        fst.as_ref().map(|s| s.to_string()).unwrap_or("*".to_string()),
        snd.as_ref().map(|s| s.to_string()).unwrap_or("*".to_string()),
      ),
    }
  }
}

impl Rule {
  pub fn to_string(&self, def_id: &DefId, def_names: &DefNames) -> String {
    format!(
      "({}{}) = {}",
      def_names.name(def_id).unwrap(),
      self.pats.iter().map(|x| format!(" {x}")).join(""),
      self.body.to_string(def_names)
    )
  }

  pub fn arity(&self) -> usize {
    self.pats.len()
  }
}

impl Definition {
  pub fn to_string(&self, def_names: &DefNames) -> String {
    self.rules.iter().map(|x| x.to_string(&self.def_id, def_names)).join("\n")
  }

  pub fn arity(&self) -> usize {
    self.rules[0].arity()
  }

  pub fn assert_no_pattern_matching_rules(&self) {
    assert!(self.rules.len() == 1, "Definition rules should have been removed in earlier pass");
  }
}

impl fmt::Display for Book {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.defs.values().map(|x| x.to_string(&self.def_names)).join("\n\n"))
  }
}

impl From<&Pattern> for Term {
  fn from(value: &Pattern) -> Self {
    match value {
      Pattern::Var(nam) => Term::Var { nam: nam.clone() },
      Pattern::Ctr(nam, pats) => Term::call(Term::Var { nam: nam.clone() }, pats.iter().map(Term::from)),
      Pattern::Num(..) => todo!(),
      Pattern::Tup(..) => todo!(),
    }
  }
}

impl fmt::Display for MatchNum {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      MatchNum::Zero => write!(f, "0"),
      MatchNum::Succ(_) => write!(f, "+"),
    }
  }
}

impl fmt::Display for Op {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Op::ADD => write!(f, "+"),
      Op::SUB => write!(f, "-"),
      Op::MUL => write!(f, "*"),
      Op::DIV => write!(f, "/"),
      Op::MOD => write!(f, "%"),
      Op::EQ => write!(f, "=="),
      Op::NE => write!(f, "!="),
      Op::LT => write!(f, "<"),
      Op::GT => write!(f, ">"),
      Op::LTE => write!(f, "<="),
      Op::GTE => write!(f, ">="),
      Op::AND => write!(f, "&"),
      Op::OR => write!(f, "|"),
      Op::XOR => write!(f, "^"),
      Op::LSH => write!(f, "<<"),
      Op::RSH => write!(f, ">>"),
      Op::NOT => write!(f, "~"),
    }
  }
}

impl fmt::Display for Name {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.0.fmt(f)
  }
}
