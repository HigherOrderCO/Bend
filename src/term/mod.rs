use self::{check::type_check::infer_match_arg_type, parser::lexer::STRINGS};
use crate::{diagnostics::Info, term::builtins::*, ENTRY_POINT};
use indexmap::{IndexMap, IndexSet};
use interner::global::GlobalString;
use itertools::Itertools;
use std::{
  borrow::Cow,
  collections::{HashMap, HashSet},
  ops::Deref,
};

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

#[derive(Debug)]
pub struct Ctx<'book> {
  pub book: &'book mut Book,
  pub info: Info,
}

impl Ctx<'_> {
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
  pub adts: Adts,

  /// To which type does each constructor belong to.
  pub ctrs: Constructors,

  /// A custom or default "main" entrypoint.
  pub entrypoint: Option<Name>,
}

pub type Adts = IndexMap<Name, Adt>;
pub type Constructors = IndexMap<Name, Name>;

/// A pattern matching function definition.
#[derive(Debug, Clone)]
pub struct Definition {
  pub name: Name,
  pub rules: Vec<Rule>,
  pub builtin: bool,
}

/// A pattern matching rule of a definition.
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct Rule {
  pub pats: Vec<Pattern>,
  pub body: Term,
}

#[derive(Debug, Default, PartialEq, Eq, Hash)]
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
    args: Vec<Term>,
    rules: Vec<Rule>,
  },
  Ref {
    nam: Name,
  },
  Era,
  #[default]
  Err,
}

impl Clone for Term {
  fn clone(&self) -> Self {
    stacker::maybe_grow(1024 * 32, 1024 * 1024, move || match self {
      Self::Lam { tag, nam, bod } => Self::Lam { tag: tag.clone(), nam: nam.clone(), bod: bod.clone() },
      Self::Var { nam } => Self::Var { nam: nam.clone() },
      Self::Chn { tag, nam, bod } => Self::Chn { tag: tag.clone(), nam: nam.clone(), bod: bod.clone() },
      Self::Lnk { nam } => Self::Lnk { nam: nam.clone() },
      Self::Let { pat, val, nxt } => Self::Let { pat: pat.clone(), val: val.clone(), nxt: nxt.clone() },
      Self::App { tag, fun, arg } => Self::App { tag: tag.clone(), fun: fun.clone(), arg: arg.clone() },
      Self::Tup { fst, snd } => Self::Tup { fst: fst.clone(), snd: snd.clone() },
      Self::Dup { tag, fst, snd, val, nxt } => {
        Self::Dup { tag: tag.clone(), fst: fst.clone(), snd: snd.clone(), val: val.clone(), nxt: nxt.clone() }
      }
      Self::Sup { tag, fst, snd } => Self::Sup { tag: tag.clone(), fst: fst.clone(), snd: snd.clone() },
      Self::Num { val } => Self::Num { val: val.clone() },
      Self::Str { val } => Self::Str { val: val.clone() },
      Self::Lst { els } => Self::Lst { els: els.clone() },
      Self::Opx { op, fst, snd } => Self::Opx { op: op.clone(), fst: fst.clone(), snd: snd.clone() },
      Self::Mat { args, rules } => Self::Mat { args: args.clone(), rules: rules.clone() },
      Self::Ref { nam } => Self::Ref { nam: nam.clone() },
      Self::Era => Self::Era,
      Self::Err => Self::Err,
    })
  }
}

impl Drop for Term {
  fn drop(&mut self) {
    if matches!(self, Term::Era | Term::Err) {
      return;
    }

    let mut stack = vec![];
    self.take_children(&mut stack);

    while let Some(mut term) = stack.pop() {
      term.take_children(&mut stack)
    }
  }
}

impl Term {
  fn take_children(&mut self, stack: &mut Vec<Term>) {
    match self {
      Term::Lam { bod, .. } | Term::Chn { bod, .. } => {
        stack.push(std::mem::take(bod.as_mut()));
      }
      Term::Let { val: fst, nxt: snd, .. }
      | Term::App { fun: fst, arg: snd, .. }
      | Term::Tup { fst, snd }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::Sup { fst, snd, .. }
      | Term::Opx { fst, snd, .. } => {
        stack.push(std::mem::take(fst.as_mut()));
        stack.push(std::mem::take(snd.as_mut()));
      }
      Term::Mat { args, rules } => {
        for arg in std::mem::take(args).into_iter() {
          stack.push(arg);
        }

        for Rule { body, .. } in std::mem::take(rules).into_iter() {
          stack.push(body);
        }
      }
      Term::Lst { els } => {
        for el in std::mem::take(els).into_iter() {
          stack.push(el);
        }
      }
      _ => {}
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pattern {
  Var(Option<Name>),
  Ctr(Name, Vec<Pattern>),
  Num(NumCtr),
  Tup(Box<Pattern>, Box<Pattern>),
  Lst(Vec<Pattern>),
  Str(GlobalString),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NumCtr {
  Num(u64),
  Succ(u64, Option<Option<Name>>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Tag {
  Named(Name),
  Numeric(u32),
  Auto,
  Static,
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
  /// Variables/wildcards.
  Any,
  /// A native tuple.
  Tup,
  /// A sequence of arbitrary numbers ending in a variable.
  Num,
  /// A strictly incrementing sequence of numbers starting from 0, ending in a + ctr.
  NumSucc(u64),
  /// Adt constructors declared with the `data` syntax.
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
  /* Common construction patterns */
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

  pub fn var_or_era(nam: Option<Name>) -> Self {
    if let Some(nam) = nam { Term::Var { nam } } else { Term::Era }
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

  pub fn str(str: &str) -> Self {
    Term::Str { val: STRINGS.get(str) }
  }

  pub fn native_num_match(arg: Term, zero: Term, succ: Term, succ_var: Option<Option<Name>>) -> Term {
    let zero = Rule { pats: vec![Pattern::Num(NumCtr::Num(0))], body: zero };
    let succ = Rule { pats: vec![Pattern::Num(NumCtr::Succ(1, succ_var))], body: succ };
    Term::Mat { args: vec![arg], rules: vec![zero, succ] }
  }

  pub fn sub_num(arg: Term, val: u64) -> Term {
    if val == 0 {
      arg
    } else {
      Term::Opx { op: Op::SUB, fst: Box::new(arg), snd: Box::new(Term::Num { val }) }
    }
  }

  pub fn add_num(arg: Term, val: u64) -> Term {
    if val == 0 {
      arg
    } else {
      Term::Opx { op: Op::ADD, fst: Box::new(arg), snd: Box::new(Term::Num { val }) }
    }
  }

  /* Common checks and transformations */

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
        if !pat.binds().contains(from) {
          nxt.subst(from, to);
        }
      }
      Term::Dup { tag: _, fst, snd, val, nxt } => {
        val.subst(from, to);
        if fst.as_ref().map_or(true, |fst| fst != from) && snd.as_ref().map_or(true, |snd| snd != from) {
          nxt.subst(from, to);
        }
      }
      Term::Mat { args, rules } => {
        for arg in args {
          arg.subst(from, to);
        }
        for rule in rules {
          if !rule.pats.iter().flat_map(|p| p.binds()).contains(from) {
            rule.body.subst(from, to);
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
      Term::Mat { args, rules } => {
        args.iter_mut().for_each(|arg| arg.subst_unscoped(from, to));
        rules.iter_mut().for_each(|rule| rule.body.subst_unscoped(from, to));
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

          for bind in pat.binds() {
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
        Term::Mat { args, rules } => {
          for arg in args {
            go(arg, free_vars);
          }
          for rule in rules {
            let mut new_scope = Default::default();
            go(&rule.body, &mut new_scope);

            for nam in rule.pats.iter().flat_map(|p| p.binds()) {
              new_scope.remove(nam);
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
        Term::Mat { args, rules } => {
          for arg in args {
            go(arg, decls, uses);
          }
          for rule in rules {
            go(&rule.body, decls, uses);
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

  pub fn is_simple_match(&self, ctrs: &Constructors, adts: &Adts) -> bool {
    // A match term
    let Term::Mat { args, rules } = self else {
      return false;
    };
    // With 1 argument
    if args.len() != 1 {
      return false;
    }
    // The argument is a variable
    if !matches!(args[0], Term::Var { .. }) {
      return false;
    }
    // Each arm only has 1 pattern for the 1 argument
    if rules.iter().any(|r| r.pats.len() != 1) {
      return false;
    }
    // The match is over a valid type
    let Ok(typ) = infer_match_arg_type(rules, 0, ctrs) else {
      return false;
    };
    // The match has one arm for each constructor, matching the constructors in adt declaration order
    match typ {
      Type::Any => {
        if rules.len() != 1 {
          return false;
        }
        if !matches!(rules[0].pats.as_slice(), [Pattern::Var(_)]) {
          return false;
        }
      }
      Type::Tup => {
        if rules.len() != 1 {
          return false;
        }
        if !matches!(rules[0].pats.as_slice(), [Pattern::Tup(box Pattern::Var(_), box Pattern::Var(_))]) {
          return false;
        }
      }
      Type::Num => {
        let mut nums = HashSet::new();
        for rule in rules {
          if let Pattern::Num(NumCtr::Num(n)) = &rule.pats[0] {
            if nums.contains(n) {
              return false;
            }
            nums.insert(*n);
          }
        }
      }
      Type::NumSucc(n) => {
        if rules.len() as u64 != n + 1 {
          return false;
        }
        for (i, _) in rules.iter().enumerate() {
          if i as u64 == n {
            let Pattern::Num(NumCtr::Succ(n_pat, Some(_))) = &rules[i].pats[0] else { return false };
            if n != *n_pat {
              return false;
            }
          } else {
            let Pattern::Num(NumCtr::Num(i_pat)) = &rules[i].pats[0] else { return false };
            if i as u64 != *i_pat {
              return false;
            }
          }
        }
      }
      Type::Adt(adt) => {
        let ctrs = &adts[&adt].ctrs;
        if rules.len() != ctrs.len() {
          return false;
        }
        for (rule, (ctr, args)) in rules.iter().zip(ctrs.iter()) {
          if let Pattern::Ctr(rule_ctr, rule_args) = &rule.pats[0] {
            if ctr != rule_ctr {
              return false;
            }
            if rule_args.len() != args.len() {
              return false;
            }
            if rule_args.iter().any(|arg| !matches!(arg, Pattern::Var(_))) {
              return false;
            }
          } else {
            return false;
          }
        }
      }
    }

    true
  }
}

impl Pattern {
  pub fn bind_or_eras(&self) -> impl DoubleEndedIterator<Item = &Option<Name>> {
    self.iter().filter_map(|pat| match pat {
      Pattern::Var(nam) => Some(nam),
      Pattern::Num(NumCtr::Succ(_, nam)) => nam.as_ref(),
      _ => None,
    })
  }

  pub fn bind_or_eras_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut Option<Name>> {
    // Can't have a Pattern::iter_mut() since it has a tree-like structure.
    fn go<'a>(pat: &'a mut Pattern, set: &mut Vec<&'a mut Option<Name>>) {
      match pat {
        Pattern::Var(nam) => set.push(nam),
        Pattern::Ctr(_, pats) | Pattern::Lst(pats) => pats.iter_mut().for_each(|pat| go(pat, set)),
        Pattern::Tup(fst, snd) => {
          go(fst, set);
          go(snd, set);
        }
        Pattern::Num(NumCtr::Succ(_, Some(nam))) => {
          set.push(nam);
        }
        Pattern::Num(_) => {}
        Pattern::Str(_) => {}
      }
    }
    let mut set = Vec::new();
    go(self, &mut set);
    set.into_iter()
  }

  pub fn binds(&self) -> impl DoubleEndedIterator<Item = &Name> {
    self.bind_or_eras().flatten()
  }

  pub fn binds_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut Name> {
    self.bind_or_eras_mut().flatten()
  }

  /// Returns an iterator over each immediate child sub-pattern of `self`.
  /// Considers Lists as its own pattern and not a sequence of Cons.
  pub fn children<'a>(&'a self) -> Box<dyn DoubleEndedIterator<Item = &'a Pattern> + 'a> {
    match self {
      Pattern::Ctr(_, children) => Box::new(children.iter()),
      Pattern::Var(_) => Box::new([].iter()),
      Pattern::Num(_) => Box::new([].iter()),
      Pattern::Tup(fst, snd) => Box::new([fst.as_ref(), snd.as_ref()].into_iter()),
      Pattern::Lst(els) => Box::new(els.iter()),
      Pattern::Str(_) => Box::new([].iter()),
    }
  }

  /// Returns an iterator over each subpattern in depth-first, left to right order.
  // TODO: Not lazy.
  pub fn iter(&self) -> impl DoubleEndedIterator<Item = &Pattern> {
    let mut to_visit = vec![self];
    let mut els = vec![];
    while let Some(pat) = to_visit.pop() {
      to_visit.extend(pat.children().rev());
      els.push(pat);
    }
    els.into_iter()
  }

  pub fn ctr_name(&self) -> Option<Name> {
    match self {
      Pattern::Var(_) => None,
      Pattern::Ctr(nam, _) => Some(nam.clone()),
      Pattern::Num(NumCtr::Num(num)) => Some(Name::new(format!("{num}"))),
      Pattern::Num(NumCtr::Succ(num, _)) => Some(Name::new(format!("{num}+"))),
      Pattern::Tup(_, _) => Some(Name::new("(,)")),
      Pattern::Lst(_) => todo!(),
      Pattern::Str(_) => todo!(),
    }
  }

  pub fn is_wildcard(&self) -> bool {
    matches!(self, Pattern::Var(_))
  }

  pub fn is_native_num_match(&self) -> bool {
    matches!(self, Pattern::Num(NumCtr::Num(0) | NumCtr::Succ(1, _)))
  }

  /// True if this pattern has no nested subpatterns.
  pub fn is_simple(&self) -> bool {
    match self {
      Pattern::Var(_) => true,
      Pattern::Ctr(_, args) => args.iter().all(|arg| matches!(arg, Pattern::Var(_))),
      Pattern::Num(_) => true,
      Pattern::Tup(fst, snd) => {
        matches!(fst.as_ref(), Pattern::Var(_)) && matches!(snd.as_ref(), Pattern::Var(_))
      }
      Pattern::Lst(_) | Pattern::Str(_) => todo!(),
    }
  }

  pub fn to_type(&self, ctrs: &Constructors) -> Type {
    match self {
      Pattern::Var(_) => Type::Any,
      Pattern::Ctr(ctr_nam, _) => {
        let adt_nam = ctrs.get(ctr_nam).expect("Unknown constructor '{ctr_nam}'");
        Type::Adt(adt_nam.clone())
      }
      Pattern::Tup(..) => Type::Tup,
      Pattern::Num(NumCtr::Num(_)) => Type::Num,
      Pattern::Num(NumCtr::Succ(n, _)) => Type::NumSucc(*n),
      Pattern::Lst(..) => Type::Adt(builtins::LIST.into()),
      Pattern::Str(..) => Type::Adt(builtins::STRING.into()),
    }
  }

  pub fn to_term(&self) -> Term {
    match self {
      Pattern::Var(nam) => Term::var_or_era(nam.clone()),
      Pattern::Ctr(ctr, args) => {
        Term::call(Term::Ref { nam: ctr.clone() }, args.iter().map(|arg| arg.to_term()))
      }
      Pattern::Num(NumCtr::Num(val)) => Term::Num { val: *val },
      // Succ constructor with no variable is not a valid term, only a compiler intermediate for a MAT inet node.
      Pattern::Num(NumCtr::Succ(_, None)) => unreachable!(),
      Pattern::Num(NumCtr::Succ(val, Some(Some(nam)))) => Term::add_num(Term::Var { nam: nam.clone() }, *val),
      Pattern::Num(NumCtr::Succ(_, Some(None))) => Term::Era,
      Pattern::Tup(fst, snd) => Term::Tup { fst: Box::new(fst.to_term()), snd: Box::new(snd.to_term()) },
      Pattern::Lst(_) | Pattern::Str(_) => todo!(),
    }
  }

  /// True if both patterns are equal (match the same expressions) without considering nested patterns.
  pub fn simple_equals(&self, other: &Pattern) -> bool {
    match (self, other) {
      (Pattern::Ctr(a, _), Pattern::Ctr(b, _)) if a == b => true,
      (Pattern::Num(NumCtr::Num(a)), Pattern::Num(NumCtr::Num(b))) if a == b => true,
      (Pattern::Num(NumCtr::Succ(a, _)), Pattern::Num(NumCtr::Succ(b, _))) if a == b => true,
      (Pattern::Tup(_, _), Pattern::Tup(_, _)) => true,
      (Pattern::Lst(_), Pattern::Lst(_)) => true,
      (Pattern::Var(_), Pattern::Var(_)) => true,
      _ => false,
    }
  }

  /// True if this pattern matches a subset of the other pattern, without considering nested patterns.
  /// That is, when something matches the ctr of self if it also matches other.
  pub fn simple_subset_of(&self, other: &Pattern) -> bool {
    self.simple_equals(other) || matches!(other, Pattern::Var(_))
  }

  /// True if the two pattern will match some common expressions.
  pub fn shares_simple_matches_with(&self, other: &Pattern) -> bool {
    self.simple_equals(other) || matches!(self, Pattern::Var(_)) || matches!(other, Pattern::Var(_))
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
  pub fn ctrs(&self, adts: &Adts) -> Vec<Pattern> {
    match self {
      Type::Any => vec![Pattern::Var(None)],
      Type::Tup => vec![Pattern::Tup(
        Box::new(Pattern::Var(Some("%fst".into()))),
        Box::new(Pattern::Var(Some("%snd".into()))),
      )],
      Type::NumSucc(n) => {
        let mut ctrs = (0 .. *n).map(|n| Pattern::Num(NumCtr::Num(n))).collect::<Vec<_>>();
        ctrs.push(Pattern::Num(NumCtr::Succ(*n, Some(Some("%pred".into())))));
        ctrs
      }
      Type::Num => unreachable!(),
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
  pub fn new<'a, V: Into<Cow<'a, str>>>(value: V) -> Name {
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

impl AsRef<str> for Name {
  fn as_ref(&self) -> &str {
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
