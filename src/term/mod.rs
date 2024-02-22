use indexmap::{IndexMap, IndexSet};
use interner::global::GlobalString;
use itertools::Itertools;
use std::{collections::HashMap, ops::Deref};

pub mod builtins;
pub mod check;
pub mod display;
pub mod load_book;
pub mod net_to_term;
pub mod parser;
pub mod term_to_net;
pub mod transform;

pub use net_to_term::{net_to_term, ReadbackError};
pub use term_to_net::{book_to_nets, term_to_compat_net};

use crate::{diagnostics::Info, term::builtins::*, ENTRY_POINT};

use self::parser::lexer::STRINGS;

#[derive(Debug)]
pub struct Ctx<'book> {
  pub book: &'book mut Book,
  pub info: Info,
}

impl<'book> Ctx<'book> {
  pub fn new(book: &mut Book) -> Ctx {
    Ctx { book, info: Info::default() }
  }
}

/// The representation of a program.
#[derive(Debug, Clone, Default)]
pub struct Book {
  /// The function definitions.
  pub defs: IndexMap<Name, Definition>,

  /// The algebraic datatypes defined by the program
  pub adts: IndexMap<Name, Adt>,

  /// To which type does each constructor belong to.
  pub ctrs: IndexMap<Name, Name>,

  /// A custom or default "main" entrypoint.
  pub entrypoint: Option<Name>,
}

/// A pattern matching function definition.
#[derive(Debug, Clone)]
pub struct Definition {
  pub name: Name,
  pub rules: Vec<Rule>,
  pub builtin: bool,
}

/// A pattern matching rule of a definition.
#[derive(Debug, Clone, Default)]
pub struct Rule {
  pub pats: Vec<Pattern>,
  pub body: Term,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MatchNum {
  Zero,
  Succ(Option<Option<Name>>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Tag {
  Named(Name),
  Numeric(u32),
  Auto,
  Static,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
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
    val: GlobalString,
  },
  Lst {
    els: Vec<Term>,
  },
  /// A numeric operation between built-in numbers.
  Opx {
    op: Op,
    fst: Box<Term>,
    snd: Box<Term>,
  },
  Mat {
    matched: Box<Term>,
    arms: Vec<(Pattern, Term)>,
  },
  Ref {
    nam: Name,
  },
  Era,
  #[default]
  Err,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pattern {
  Var(Option<Name>),
  Ctr(Name, Vec<Pattern>),
  Num(MatchNum),
  Tup(Box<Pattern>, Box<Pattern>),
  Lst(Vec<Pattern>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
  pub builtin: bool,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum AdtEncoding {
  Scott,

  #[default]
  TaggedScott,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Name(GlobalString);

impl PartialEq<str> for Name {
  fn eq(&self, other: &str) -> bool {
    &**self == other
  }
}

pub fn num_to_name(mut num: u64) -> String {
  let mut name = String::new();
  loop {
    let c = (num % 26) as u8 + b'a';
    name.push(c as char);
    num /= 26;
    if num == 0 {
      break;
    }
  }
  name
}

impl Tag {
  pub fn adt_name(name: &Name) -> Self {
    Self::Named(name.clone())
  }

  pub fn adt_field(adt: &Name, ctr: &Name, field: &Name) -> Self {
    Self::Named(Name::new(format!("{adt}.{ctr}.{field}")))
  }
}

impl Term {
  pub fn lam(nam: Option<Name>, bod: Term) -> Self {
    Term::Lam { tag: Tag::Static, nam, bod: Box::new(bod) }
  }

  pub fn named_lam(nam: Name, bod: Term) -> Self {
    Term::Lam { tag: Tag::Static, nam: Some(nam), bod: Box::new(bod) }
  }

  pub fn erased_lam(bod: Term) -> Self {
    Term::Lam { tag: Tag::Static, nam: None, bod: Box::new(bod) }
  }

  pub fn tagged_lam(tag: Tag, nam: Name, bod: Term) -> Self {
    Term::Lam { tag, nam: Some(nam), bod: Box::new(bod) }
  }

  pub fn app(fun: Term, arg: Term) -> Self {
    Term::App { tag: Tag::Static, fun: Box::new(fun), arg: Box::new(arg) }
  }

  pub fn tagged_app(tag: Tag, fun: Term, arg: Term) -> Self {
    Term::App { tag, fun: Box::new(fun), arg: Box::new(arg) }
  }

  /// Make a call term by folding args around a called function term with applications.
  pub fn call(called: Term, args: impl IntoIterator<Item = Term>) -> Self {
    args.into_iter().fold(called, Term::app)
  }

  pub fn tagged_call(tag: Tag, called: Term, args: impl IntoIterator<Item = Term>) -> Self {
    args.into_iter().fold(called, |acc, arg| Term::tagged_app(tag.clone(), acc, arg))
  }

  /// Apply a variable to a term by the var name.
  pub fn arg_call(fun: Term, arg: Name) -> Self {
    Term::app(fun, Term::Var { nam: arg })
  }

  pub fn r#ref(name: &str) -> Self {
    Term::Ref { nam: name.into() }
  }

  /// Substitute the occurrences of a variable in a term with the given term.
  /// Caution: can cause invalid shadowing of variables if used incorrectly.
  /// Ex: Using subst to beta-reduce (@a @b a b) converting it into (@b b).
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
        if !pat.names().contains(from) {
          nxt.subst(from, to);
        }
      }
      Term::Dup { tag: _, fst, snd, val, nxt } => {
        val.subst(from, to);
        if fst.as_ref().map_or(true, |fst| fst != from) && snd.as_ref().map_or(true, |snd| snd != from) {
          nxt.subst(from, to);
        }
      }
      Term::Mat { matched, arms } => {
        matched.subst(from, to);

        for (rule, term) in arms {
          let can_subst;

          if let Pattern::Num(MatchNum::Succ(Some(Some(nam)))) = rule {
            can_subst = nam != from;
          } else {
            can_subst = true;
          };

          if can_subst {
            term.subst(from, to);
          }
        }
      }
      Term::Lst { els } => els.iter_mut().for_each(|el| el.subst(from, to)),
      Term::App { fun: fst, arg: snd, .. }
      | Term::Sup { fst, snd, .. }
      | Term::Tup { fst, snd }
      | Term::Opx { fst, snd, .. } => {
        fst.subst(from, to);
        snd.subst(from, to);
      }
      Term::Ref { .. } | Term::Num { .. } | Term::Str { .. } | Term::Era | Term::Err => (),
    }
  }

  /// Substitute the occurrence of an unscoped variable with the given term.
  pub fn subst_unscoped(&mut self, from: &Name, to: &Term) {
    match self {
      Term::Lnk { nam } if nam == from => {
        *self = to.clone();
      }
      Term::Mat { matched, arms } => {
        matched.subst_unscoped(from, to);
        arms.iter_mut().for_each(|(_, arm)| arm.subst_unscoped(from, to));
      }
      Term::Lst { els } => els.iter_mut().for_each(|el| el.subst_unscoped(from, to)),
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
      | Term::Era
      | Term::Err => (),
    }
  }

  /// Collects all the free variables that a term has
  /// and the number of times each var is used
  pub fn free_vars(&self) -> HashMap<Name, u64> {
    fn go(term: &Term, free_vars: &mut HashMap<Name, u64>) {
      match term {
        Term::Lam { nam: Some(nam), bod, .. } => {
          let mut new_scope = Default::default();
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

          let mut new_scope = Default::default();
          go(nxt, &mut new_scope);

          for bind in pat.names() {
            new_scope.remove(bind);
          }

          free_vars.extend(new_scope);
        }
        Term::Dup { fst, snd, val, nxt, .. } => {
          go(val, free_vars);

          let mut new_scope = Default::default();
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
        Term::Mat { matched, arms } => {
          go(matched, free_vars);

          for (rule, term) in arms {
            let mut new_scope = Default::default();
            go(term, &mut new_scope);

            for var in rule.names() {
              new_scope.remove(var);
            }

            free_vars.extend(new_scope);
          }
        }
        Term::Lst { els } => {
          for el in els {
            let mut fvs = Default::default();
            go(el, &mut fvs);
            free_vars.extend(fvs);
          }
        }
        Term::Ref { .. } | Term::Num { .. } | Term::Str { .. } | Term::Era | Term::Err => {}
      }
    }

    let mut free_vars = Default::default();
    go(self, &mut free_vars);
    free_vars
  }

  /// Returns the set of declared and the set of used unscoped variables
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
        Term::Mat { matched, arms } => {
          go(matched, decls, uses);
          for (_, arm) in arms {
            go(arm, decls, uses);
          }
        }
        Term::Lst { els } => {
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
          go(snd, decls, uses);
        }
        Term::Lam { bod, .. } => {
          go(bod, decls, uses);
        }
        Term::Var { .. } | Term::Num { .. } | Term::Str { .. } | Term::Ref { .. } | Term::Era | Term::Err => {
        }
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
        let new_label = Name::new(format!("{}-1", nam));
        succ_term.subst(label, &Term::Var { nam: new_label.clone() });
        succ_label = Some(new_label);
      }

      let succ = (Pattern::Num(MatchNum::Succ(Some(succ_label))), succ_term);
      Term::Mat { matched: Box::new(scrutinee), arms: vec![zero, succ] }
    } else {
      match succ_label {
        Some(succ) => {
          let match_bind = succ.clone();

          let new_label = Name::new(format!("{}-1", succ));
          succ_term.subst(&succ, &Term::Var { nam: new_label.clone() });
          succ_label = Some(new_label);

          let succ = (Pattern::Num(MatchNum::Succ(Some(succ_label))), succ_term);

          Term::Let {
            pat: Pattern::Var(Some(match_bind.clone())),
            val: Box::new(scrutinee),
            nxt: Box::new(Term::Mat {
              matched: Box::new(Term::Var { nam: match_bind }),
              arms: vec![zero, succ],
            }),
          }
        }
        None => {
          let succ = (Pattern::Num(MatchNum::Succ(None)), succ_term);

          Term::Mat { matched: Box::new(scrutinee), arms: vec![zero, succ] }
        }
      }
    }
  }
}

impl Pattern {
  pub fn vars(&self) -> impl DoubleEndedIterator<Item = &Option<Name>> {
    fn go<'a>(pat: &'a Pattern, set: &mut Vec<&'a Option<Name>>) {
      match pat {
        Pattern::Var(nam) => set.push(nam),
        Pattern::Ctr(_, pats) | Pattern::Lst(pats) => pats.iter().for_each(|pat| go(pat, set)),
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
    set.into_iter()
  }

  pub fn vars_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut Option<Name>> {
    fn go<'a>(pat: &'a mut Pattern, set: &mut Vec<&'a mut Option<Name>>) {
      match pat {
        Pattern::Var(nam) => set.push(nam),
        Pattern::Ctr(_, pats) | Pattern::Lst(pats) => pats.iter_mut().for_each(|pat| go(pat, set)),
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
    set.into_iter()
  }

  pub fn names(&self) -> impl DoubleEndedIterator<Item = &Name> {
    self.vars().flatten()
  }

  pub fn names_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut Name> {
    self.vars_mut().flatten()
  }

  pub fn ctrs(&self) -> Vec<Name> {
    fn go(pat: &Pattern, set: &mut Vec<Name>) {
      match pat {
        Pattern::Ctr(nam, pats) => {
          set.push(nam.clone());
          pats.iter().for_each(|pat| go(pat, set));
        }
        Pattern::Lst(pats) => {
          set.push(builtins::LCONS.into());
          set.push(builtins::LNIL.into());
          pats.iter().for_each(|pat| go(pat, set))
        }
        Pattern::Tup(fst, snd) => {
          set.push("(,)".into());
          go(fst, set);
          go(snd, set);
        }
        Pattern::Num(MatchNum::Zero) => {
          set.push("0".into());
        }
        Pattern::Num(MatchNum::Succ(_)) => {
          set.push("+".into());
        }
        Pattern::Var(_) => {}
      }
    }
    let mut set = Vec::new();
    go(self, &mut set);
    set
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

  /// True if this pattern has no nested subpatterns.
  pub fn is_flat(&self) -> bool {
    match self {
      Pattern::Var(_) => true,
      Pattern::Ctr(_, args) | Pattern::Lst(args) => args.iter().all(|arg| matches!(arg, Pattern::Var(_))),
      Pattern::Num(_) => true,
      Pattern::Tup(fst, snd) => {
        matches!(fst.as_ref(), Pattern::Var(_)) && matches!(snd.as_ref(), Pattern::Var(_))
      }
    }
  }

  pub fn to_type(&self, ctrs: &IndexMap<Name, Name>) -> Type {
    match self {
      Pattern::Var(_) => Type::Any,
      Pattern::Ctr(ctr_nam, _) => {
        let adt_nam = ctrs.get(ctr_nam).expect("Unknown constructor '{ctr_nam}'");
        Type::Adt(adt_nam.clone())
      }
      Pattern::Tup(..) => Type::Tup,
      Pattern::Num(..) => Type::Num,
      Pattern::Lst(..) => Type::Adt(builtins::LIST.into()),
    }
  }

  pub fn to_term(&self) -> Term {
    match self {
      Pattern::Var(None) => Term::Era,
      Pattern::Var(Some(nam)) => Term::Var { nam: nam.clone() },
      Pattern::Ctr(ctr, args) => {
        Term::call(Term::Ref { nam: ctr.clone() }, args.iter().map(|arg| arg.to_term()))
      }
      Pattern::Num(MatchNum::Zero) => Term::Num { val: 0 },
      // Succ constructor with no variable is not a valid term, only a compiler intermediate for a MAT inet node.
      Pattern::Num(MatchNum::Succ(None)) => unreachable!(),
      Pattern::Num(MatchNum::Succ(Some(Some(nam)))) => Term::Opx {
        op: Op::ADD,
        fst: Box::new(Term::Var { nam: nam.clone() }),
        snd: Box::new(Term::Num { val: 1 }),
      },
      Pattern::Num(MatchNum::Succ(Some(None))) => Term::Era,
      Pattern::Tup(fst, snd) => Term::Tup { fst: Box::new(fst.to_term()), snd: Box::new(snd.to_term()) },
      Pattern::Lst(_) => {
        let mut p = self.clone();
        p.encode_builtins();
        p.to_term()
      }
    }
  }

  /// True if both patterns are equal (match the same expressions) without considering nested patterns.
  pub fn flat_equals(&self, other: &Pattern) -> bool {
    match (self, other) {
      (Pattern::Ctr(a, _), Pattern::Ctr(b, _)) if a == b => true,
      (Pattern::Num(MatchNum::Zero), Pattern::Num(MatchNum::Zero)) => true,
      (Pattern::Num(MatchNum::Succ(_)), Pattern::Num(MatchNum::Succ(_))) => true,
      (Pattern::Tup(_, _), Pattern::Tup(_, _)) => true,
      (Pattern::Lst(_), Pattern::Lst(_)) => true,
      (Pattern::Var(_), Pattern::Var(_)) => true,
      _ => false,
    }
  }

  /// True if this pattern matches a subset of the other pattern, without considering nested patterns.
  /// That is, when something matches the ctr of self if it also matches other.
  pub fn is_flat_subset_of(&self, other: &Pattern) -> bool {
    self.flat_equals(other) || matches!(other, Pattern::Var(_))
  }

  /// True if the two pattern will match some common expressions.
  pub fn shares_matches_with(&self, other: &Pattern) -> bool {
    self.flat_equals(other) || matches!(self, Pattern::Var(_)) || matches!(other, Pattern::Var(_))
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

  #[track_caller]
  pub fn assert_no_pattern_matching_rules(&self) {
    assert!(self.rules.len() == 1, "Definition rules should have been removed in earlier pass");
    assert!(self.rules[0].pats.is_empty(), "Definition args should have been removed in an earlier pass");
  }

  #[track_caller]
  pub fn rule(&self) -> &Rule {
    self.assert_no_pattern_matching_rules();
    &self.rules[0]
  }

  #[track_caller]
  pub fn rule_mut(&mut self) -> &mut Rule {
    self.assert_no_pattern_matching_rules();
    &mut self.rules[0]
  }
}

impl Type {
  /// Return the constructors for a given type as patterns.
  pub fn ctrs(&self, adts: &IndexMap<Name, Adt>) -> Vec<Pattern> {
    match self {
      Type::None => vec![],
      Type::Any => vec![],
      Type::Tup => vec![Pattern::Tup(
        Box::new(Pattern::Var(Some("%fst".into()))),
        Box::new(Pattern::Var(Some("%snd".into()))),
      )],
      Type::Num => {
        vec![Pattern::Num(MatchNum::Zero), Pattern::Num(MatchNum::Succ(Some(Some("%pred".into()))))]
      }
      Type::Adt(adt) => {
        // TODO: Should return just a ref to ctrs and not clone.
        adts[adt]
          .ctrs
          .iter()
          .map(|(nam, args)| {
            Pattern::Ctr(nam.clone(), args.iter().map(|x| Pattern::Var(Some(x.clone()))).collect())
          })
          .collect()
      }
    }
  }

  /// True if the type is an ADT or a builtin equivalent of an ADT (like tups and numbers)
  pub fn is_ctr_type(&self) -> bool {
    matches!(self, Type::Adt(_) | Type::Num | Type::Tup)
  }

  pub fn is_var_type(&self) -> bool {
    matches!(self, Type::Any)
  }
}

impl Name {
  /// For constructing a name when you have an owned String.  
  /// If you have an `&str` prefer to use `Name::From<&str>`
  pub fn new(value: String) -> Name {
    Name(STRINGS.get(value))
  }

  pub fn is_generated(&self) -> bool {
    // Generated def names use $ while var names use %
    self.contains('$') || self.contains('%')
  }
}

impl From<&str> for Name {
  fn from(value: &str) -> Self {
    Name(STRINGS.get(value))
  }
}

impl From<u64> for Name {
  fn from(value: u64) -> Self {
    num_to_name(value).as_str().into()
  }
}

impl From<u32> for Name {
  fn from(value: u32) -> Self {
    num_to_name(value as u64).as_str().into()
  }
}

impl Deref for Name {
  type Target = str;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl Book {
  pub fn hvmc_entrypoint(&self) -> &str {
    match self.entrypoint.as_ref().map(|e| e.as_ref()) {
      Some("main" | "Main") | None => ENTRY_POINT,
      Some(nam) => nam,
    }
  }
}
