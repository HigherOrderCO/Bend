use hvmc::run::Val;
use indexmap::{IndexMap, IndexSet};
use shrinkwraprs::Shrinkwrap;
use std::collections::{BTreeMap, HashMap};

pub mod check;
pub mod display;
pub mod load_book;
pub mod net_to_term;
pub mod parser;
pub mod term_to_net;
pub mod transform;
pub mod builtin_adt;

pub use net_to_term::{net_to_term, ReadbackError};
pub use term_to_net::{book_to_nets, term_to_compat_net};

use self::transform::encode_lists;

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
  pub generated: bool,
  pub rules: Vec<Rule>,
}

/// A pattern matching rule of a definition.
#[derive(Debug, Clone, Default)]
pub struct Rule {
  pub pats: Vec<Pattern>,
  pub body: Term,
}

#[derive(Debug, Clone)]
pub enum MatchNum {
  Zero,
  Succ(Option<Option<Name>>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Tag {
  Named(Name),
  Numeric(u32),
  Auto,
  Static,
}

#[derive(Debug, Clone, Default)]
pub enum Term {
  Lam {
    tag: Tag,
    nam: Option<Name>,
    bod: Box<Term>,
  },
  Var {
    nam: Name,
  },
  /// Like a scopeless lambda, where the variable can occur outside the body
  Chn {
    tag: Tag,
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
    tag: Tag,
    fun: Box<Term>,
    arg: Box<Term>,
  },
  Tup {
    fst: Box<Term>,
    snd: Box<Term>,
  },
  Dup {
    tag: Tag,
    fst: Option<Name>,
    snd: Option<Name>,
    val: Box<Term>,
    nxt: Box<Term>,
  },
  Sup {
    tag: Tag,
    fst: Box<Term>,
    snd: Box<Term>,
  },
  Num {
    val: u64,
  },
  Str {
    val: String,
  },
  List {
    els: Vec<Term>,
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
  Var(Option<Name>),
  Ctr(Name, Vec<Pattern>),
  Num(MatchNum),
  Tup(Box<Pattern>, Box<Pattern>),
  List(Vec<Pattern>),
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

/// Pattern types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
  None,
  Any,
  Tup,
  Num,
  Adt(Name),
}

/// A user defined datatype
#[derive(Debug, Clone, Default)]
pub struct Adt {
  pub ctrs: IndexMap<Name, Vec<Name>>,
}

#[derive(Debug, PartialEq, Eq, Clone, Shrinkwrap, Hash, PartialOrd, Ord, Default)]
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

impl Tag {
  pub fn string() -> Self {
    Self::Named(Name::new("String"))
  }

  pub fn string_scons_head() -> Self {
    Self::Named(Name::new("String.SCons.head"))
  }

  pub fn list() -> Self {
    Self::Named(Name::new("List"))
  }

  pub fn list_lcons_head() -> Self {
    Self::Named(Name::new("List.LCons.head"))
  }
}

impl Book {
  pub fn new() -> Self {
    Default::default()
  }

  pub fn insert_def(&mut self, name: Name, is_generated: bool, rules: Vec<Rule>) -> DefId {
    let def_id = self.def_names.insert(name);
    let def = Definition { def_id, generated: is_generated, rules };
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
  /// Make a call term by folding args around a called function term with applications.
  pub fn call(called: Term, args: impl IntoIterator<Item = Term>) -> Self {
    args.into_iter().fold(called, |acc, arg| Term::App {
      tag: Tag::Static,
      fun: Box::new(acc),
      arg: Box::new(arg),
    })
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
      Term::Let { pat, val, nxt } => {
        val.subst(from, to);
        if !pat.occurs(from) {
          nxt.subst(from, to);
        }
      }
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

          if let Pattern::Num(MatchNum::Succ(Some(Some(nam)))) = rule {
            can_subst = nam != from
          } else {
            can_subst = true
          };

          if can_subst {
            term.subst(from, to);
          }
        }
      }
      Term::List { els } => els.iter_mut().for_each(|el| el.subst(from, to)),
      Term::App { fun: fst, arg: snd, .. }
      | Term::Sup { fst, snd, .. }
      | Term::Tup { fst, snd }
      | Term::Opx { fst, snd, .. } => {
        fst.subst(from, to);
        snd.subst(from, to);
      }
      Term::Ref { .. } | Term::Num { .. } | Term::Str { .. } | Term::Era => (),
    }
  }

  /// Substitute the occurence of an unscoped variable with the given term.
  pub fn subst_unscoped(&mut self, from: &Name, to: &Term) {
    match self {
      Term::Lnk { nam } if nam == from => {
        *self = to.clone();
      }
      Term::Match { scrutinee, arms } => {
        scrutinee.subst_unscoped(from, to);
        arms.iter_mut().for_each(|(_, arm)| arm.subst_unscoped(from, to));
      }
      Term::List { els } => els.iter_mut().for_each(|el| el.subst_unscoped(from, to)),
      Term::Chn { bod, .. } | Term::Lam { bod, .. } => bod.subst_unscoped(from, to),
      Term::App { fun: fst, arg: snd, .. }
      | Term::Let { val: fst, nxt: snd, .. }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::Sup { fst, snd, .. }
      | Term::Tup { fst, snd }
      | Term::Opx { fst, snd, .. } => {
        fst.subst(from, to);
        snd.subst(from, to);
      }
      Term::Var { .. }
      | Term::Lnk { .. }
      | Term::Ref { .. }
      | Term::Num { .. }
      | Term::Str { .. }
      | Term::Era => (),
    }
  }

  /// Collects all the free variables that a term has
  /// and the number of times each var is used
  pub fn free_vars(&self) -> IndexMap<Name, u64> {
    fn go(term: &Term, free_vars: &mut IndexMap<Name, u64>) {
      match term {
        Term::Lam { nam: Some(nam), bod, .. } => {
          let mut new_scope = IndexMap::new();
          go(bod, &mut new_scope);
          new_scope.remove(nam);

          free_vars.extend(new_scope);
        }
        Term::Lam { nam: None, bod, .. } => go(bod, free_vars),
        Term::Var { nam } => *free_vars.entry(nam.clone()).or_default() += 1,
        Term::Chn { bod, .. } => go(bod, free_vars),
        Term::Lnk { .. } => {}
        Term::Let { pat, val, nxt } => {
          go(val, free_vars);

          let mut new_scope = IndexMap::new();
          go(nxt, &mut new_scope);

          for bind in pat.names() {
            new_scope.remove(bind);
          }

          free_vars.extend(new_scope);
        }
        Term::Dup { fst, snd, val, nxt, .. } => {
          go(val, free_vars);

          let mut new_scope = IndexMap::new();
          go(nxt, &mut new_scope);

          fst.as_ref().map(|fst| new_scope.remove(fst));
          snd.as_ref().map(|snd| new_scope.remove(snd));

          free_vars.extend(new_scope);
        }
        Term::App { fun: fst, arg: snd, .. }
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

            if let Pattern::Num(MatchNum::Succ(Some(Some(nam)))) = rule {
              new_scope.remove(nam);
            }

            free_vars.extend(new_scope);
          }
        }
        Term::List { els } => {
          for el in els {
            let mut fvs = IndexMap::new();
            go(el, &mut fvs);
            free_vars.extend(fvs);
          }
        }
        Term::Ref { .. } | Term::Num { .. } | Term::Str { .. } | Term::Era => {}
      }
    }

    let mut free_vars = IndexMap::new();
    go(self, &mut free_vars);
    free_vars
  }

  pub fn unscoped_vars(&self) -> (IndexSet<Name>, IndexSet<Name>) {
    fn go(term: &Term, decls: &mut IndexSet<Name>, uses: &mut IndexSet<Name>) {
      match term {
        Term::Chn { tag: _, nam, bod } => {
          decls.insert(nam.clone());
          go(bod, decls, uses);
        }
        Term::Lnk { nam } => {
          uses.insert(nam.clone());
        }
        Term::Match { scrutinee, arms } => {
          go(scrutinee, decls, uses);
          for (_, arm) in arms {
            go(arm, decls, uses);
          }
        }
        Term::List { els } => {
          for el in els {
            go(el, decls, uses);
          }
        }
        Term::Let { val: fst, nxt: snd, .. }
        | Term::App { fun: fst, arg: snd, .. }
        | Term::Tup { fst, snd }
        | Term::Dup { val: fst, nxt: snd, .. }
        | Term::Sup { fst, snd, .. }
        | Term::Opx { fst, snd, .. } => {
          go(fst, decls, uses);
          go(snd, uses, decls);
        }
        Term::Lam { bod, .. } => {
          go(bod, decls, uses);
        }
        Term::Var { .. } | Term::Num { .. } | Term::Str { .. } | Term::Ref { .. } | Term::Era => (),
      }
    }
    let mut decls = Default::default();
    let mut uses = Default::default();
    go(self, &mut decls, &mut uses);
    (decls, uses)
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

      let succ = (Pattern::Num(MatchNum::Succ(Some(succ_label))), succ_term);
      Term::Match { scrutinee: Box::new(scrutinee), arms: vec![zero, succ] }
    } else {
      let match_bind = succ_label.clone().unwrap_or_else(|| Name::new("*"));

      if let Some(label) = &succ_label {
        let new_label = Name(format!("{}-1", label));
        succ_term.subst(label, &Term::Var { nam: new_label.clone() });
        succ_label = Some(new_label);
      }

      let succ = (Pattern::Num(MatchNum::Succ(Some(succ_label))), succ_term);

      Term::Let {
        pat: Pattern::Var(Some(match_bind.clone())),
        val: Box::new(scrutinee),
        nxt: Box::new(Term::Match {
          scrutinee: Box::new(Term::Var { nam: match_bind }),
          arms: vec![zero, succ],
        }),
      }
    }
  }
}

impl Pattern {
  pub fn occurs(&self, name: &Name) -> bool {
    match self {
      Pattern::Var(None) => false,
      Pattern::Var(Some(nam)) => nam == name,
      Pattern::Ctr(.., args) | Pattern::List(args) => {
        let mut ret = false;
        for arg in args {
          ret |= arg.occurs(name);
        }
        ret
      }
      Pattern::Num(..) => false,
      Pattern::Tup(fst, snd) => fst.occurs(name) || snd.occurs(name),
    }
  }

  pub fn names(&self) -> impl DoubleEndedIterator<Item = &Name> {
    fn go<'a>(pat: &'a Pattern, set: &mut Vec<&'a Option<Name>>) {
      match pat {
        Pattern::Var(nam) => set.push(nam),
        Pattern::Ctr(_, pats) | Pattern::List(pats) => pats.iter().for_each(|pat| go(pat, set)),
        Pattern::Tup(fst, snd) => {
          go(fst, set);
          go(snd, set);
        }
        Pattern::Num(MatchNum::Succ(Some(nam))) => {
          set.push(nam);
        }
        Pattern::Num(_) => {}
      }
    }
    let mut set = Vec::new();
    go(self, &mut set);
    set.into_iter().flat_map(|a| a.as_ref())
  }

  pub fn names_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut Name> {
    fn go<'a>(pat: &'a mut Pattern, set: &mut Vec<&'a mut Option<Name>>) {
      match pat {
        Pattern::Var(nam) => set.push(nam),
        Pattern::Ctr(_, pats) | Pattern::List(pats) => pats.iter_mut().for_each(|pat| go(pat, set)),
        Pattern::Tup(fst, snd) => {
          go(fst, set);
          go(snd, set);
        }
        Pattern::Num(MatchNum::Succ(Some(nam))) => {
          set.push(nam);
        }
        Pattern::Num(_) => {}
      }
    }
    let mut set = Vec::new();
    go(self, &mut set);
    set.into_iter().flat_map(|a| a.as_mut())
  }

  pub fn is_detached_num_match(&self) -> bool {
    if let Pattern::Num(num) = self {
      match num {
        MatchNum::Zero => true,
        MatchNum::Succ(None) => true,
        MatchNum::Succ(Some(_)) => false,
      }
    } else {
      false
    }
  }

  pub fn is_flat(&self) -> bool {
    match self {
      Pattern::Var(_) => true,
      Pattern::Ctr(_, args) | Pattern::List(args) => args.iter().all(|arg| matches!(arg, Pattern::Var(_))),
      Pattern::Num(_) => true,
      Pattern::Tup(fst, snd) => {
        matches!(fst.as_ref(), Pattern::Var(_)) && matches!(snd.as_ref(), Pattern::Var(_))
      }
    }
  }

  pub fn to_type(&self, ctrs: &HashMap<Name, Name>) -> Result<Type, String> {
    let typ = match self {
      Pattern::Var(_) => Type::Any,
      Pattern::Ctr(ctr_nam, _) => {
        if let Some(adt_nam) = ctrs.get(ctr_nam) {
          Type::Adt(adt_nam.clone())
        } else {
          return Err(format!("Unknown constructor '{ctr_nam}'"));
        }
      }
      Pattern::Tup(..) => Type::Tup,
      Pattern::Num(..) => Type::Num,
      Pattern::List(..) => Type::Adt(Name::new(encode_lists::LIST)),
    };
    Ok(typ)
  }
}

impl Rule {
  pub fn arity(&self) -> usize {
    self.pats.len()
  }
}

impl Definition {
  pub fn arity(&self) -> usize {
    self.rules[0].arity()
  }

  pub fn assert_no_pattern_matching_rules(&self) {
    assert!(self.rules.len() == 1, "Definition rules should have been removed in earlier pass");
    assert!(self.rules[0].pats.is_empty(), "Definition args should have been removed in an earlier pass");
  }
}

impl From<&Pattern> for Term {
  fn from(value: &Pattern) -> Self {
    match value {
      Pattern::Var(None) => Term::Era,
      Pattern::Var(Some(nam)) => Term::Var { nam: nam.clone() },
      Pattern::Ctr(nam, pats) => Term::call(Term::Var { nam: nam.clone() }, pats.iter().map(Term::from)),
      Pattern::Num(..) => todo!(),
      Pattern::Tup(..) => todo!(),
      Pattern::List(..) => todo!(),
    }
  }
}
