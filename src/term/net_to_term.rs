use super::{term_to_net::Labels, var_id_to_name, Book, DefId, MatchNum, Name, Op, Tag, Term, Val};
use crate::{
  net::{INet, NodeId, NodeKind::*, Port, SlotId, ROOT},
  term::Pattern,
};
use hvmc::run::Loc;
use indexmap::IndexSet;
use std::collections::{HashMap, HashSet};

// TODO: Display scopeless lambdas as such
/// Converts an Interaction-INet to a Lambda Calculus term
pub fn net_to_term(net: &INet, book: &Book, labels: &Labels, linear: bool) -> (Term, Vec<ReadbackError>) {
  let mut reader = Reader {
    net,
    labels,
    book,
    dup_paths: if linear { None } else { Some(Default::default()) },
    scope: Default::default(),
    namegen: Default::default(),
    seen: Default::default(),
    errors: Default::default(),
  };

  let mut term = reader.read_term(net.enter_port(ROOT));

  while let Some(node) = reader.scope.vec.pop() {
    let val = reader.read_term(reader.net.enter_port(Port(node, 0)));
    let fst = reader.namegen.decl_name(net, Port(node, 1));
    let snd = reader.namegen.decl_name(net, Port(node, 2));
    let mut free_vars = val.free_vars().into_keys().collect();

    let tag = match reader.net.node(node).kind {
      Tup => None,
      Dup { lab } => Some(reader.labels.dup.to_tag(Some(lab))),
      _ => unreachable!(),
    };
    let let_ctx = LetInsertion::Todo(tag, fst, snd, val);

    match let_ctx.search_and_insert(&mut term, &mut free_vars).0 {
      LetInsertion::Err(kind, fst, snd, val) => {
        let result = term.insert_let(kind, fst, snd, val, &mut IndexSet::new());
        debug_assert!(matches!(result, LetInsertion::Ok));
      }
      LetInsertion::Ok => {}
      _ => unreachable!(),
    }
  }

  (term, reader.errors)
}

#[derive(Debug)]
pub enum ReadbackError {
  InvalidNumericMatch,
  ReachedRoot,
  Cyclic,
  InvalidBind,
}

struct Reader<'a> {
  net: &'a INet,
  labels: &'a Labels,
  book: &'a Book,
  dup_paths: Option<HashMap<u32, Vec<SlotId>>>,
  scope: Scope,
  namegen: NameGen,
  seen: HashSet<Port>,
  errors: Vec<ReadbackError>,
}

impl<'a> Reader<'a> {
  fn read_term(&mut self, next: Port) -> Term {
    if self.dup_paths.is_none() && !self.seen.insert(next) {
      self.errors.push(ReadbackError::Cyclic);
      return Term::Var { nam: Name::new("...") };
    }

    let node = next.node();

    let term = match self.net.node(node).kind {
      Era => {
        // Only the main port actually exists in an ERA, the auxes are just an artifact of this representation.
        debug_assert!(next.slot() == 0);
        Term::Era
      }
      // If we're visiting a con node...
      Con { lab } => match next.slot() {
        // If we're visiting a port 0, then it is a lambda.
        0 => {
          let nam = self.namegen.decl_name(self.net, Port(node, 1));
          let bod = self.read_term(self.net.enter_port(Port(node, 2)));
          Term::Lam { tag: self.labels.con.to_tag(lab), nam, bod: Box::new(bod) }
        }
        // If we're visiting a port 1, then it is a variable.
        1 => Term::Var { nam: self.namegen.var_name(next) },
        // If we're visiting a port 2, then it is an application.
        2 => {
          let fun = self.read_term(self.net.enter_port(Port(node, 0)));
          let arg = self.read_term(self.net.enter_port(Port(node, 1)));
          Term::App { tag: self.labels.con.to_tag(lab), fun: Box::new(fun), arg: Box::new(arg) }
        }
        _ => unreachable!(),
      },
      Mat => match next.slot() {
        2 => {
          // Read the matched expression
          let scrutinee = self.read_term(self.net.enter_port(Port(node, 0)));

          // Read the pattern matching node
          let sel_node = self.net.enter_port(Port(node, 1)).node();

          // We expect the pattern matching node to be a CON
          let sel_kind = self.net.node(sel_node).kind;
          if sel_kind != (Con { lab: None }) {
            // TODO: Is there any case where we expect a different node type here on readback?
            self.errors.push(ReadbackError::InvalidNumericMatch);
            Term::new_native_match(scrutinee, Term::Era, None, Term::Era)
          } else {
            let zero_term = self.read_term(self.net.enter_port(Port(sel_node, 1)));
            let succ_term = self.read_term(self.net.enter_port(Port(sel_node, 2)));

            let Term::Lam { nam, bod, .. } = succ_term else { unreachable!() };

            Term::new_native_match(scrutinee, zero_term, nam, *bod)
          }
        }
        _ => unreachable!(),
      },
      Ref { def_id } => {
        if self.book.is_generated_def(def_id) {
          let def = self.book.defs.get(&def_id).unwrap();
          def.assert_no_pattern_matching_rules();
          let mut term = def.rules[0].body.clone();
          term.fix_names(&mut self.namegen.id_counter, self.book);

          term
        } else {
          Term::Ref { def_id }
        }
      }
      // If we're visiting a fan node...
      Dup { lab } => match next.slot() {
        // If we're visiting a port 0, then it is a pair.
        0 => {
          if let Some(dup_paths) = &mut self.dup_paths {
            let stack = dup_paths.entry(lab).or_default();
            if let Some(slot) = stack.pop() {
              // Since we had a paired Dup in the path to this Sup,
              // we "decay" the superposition according to the original direction we came from the Dup.
              let term = self.read_term(self.net.enter_port(Port(node, slot)));
              self.dup_paths.as_mut().unwrap().get_mut(&lab).unwrap().push(slot);
              Some(term)
            } else {
              None
            }
          } else {
            None
          }
          .unwrap_or_else(|| {
            // If no Dup with same label in the path, we can't resolve the Sup, so keep it as a term.
            let fst = self.read_term(self.net.enter_port(Port(node, 1)));
            let snd = self.read_term(self.net.enter_port(Port(node, 2)));
            Term::Sup { tag: self.labels.dup.to_tag(Some(lab)), fst: Box::new(fst), snd: Box::new(snd) }
          })
        }
        // If we're visiting a port 1 or 2, then it is a variable.
        // Also, that means we found a dup, so we store it to read later.
        1 | 2 => {
          if let Some(dup_paths) = &mut self.dup_paths {
            dup_paths.entry(lab).or_default().push(next.slot());
            let term = self.read_term(self.net.enter_port(Port(node, 0)));
            self.dup_paths.as_mut().unwrap().entry(lab).or_default().pop().unwrap();
            term
          } else {
            self.scope.insert(node);
            Term::Var { nam: self.namegen.var_name(next) }
          }
        }
        _ => unreachable!(),
      },
      Num { val } => Term::Num { val },
      Op2 { opr } => match next.slot() {
        2 => {
          let fst = self.read_term(self.net.enter_port(Port(node, 0)));
          let snd = self.read_term(self.net.enter_port(Port(node, 1)));

          Term::Opx { op: Op::from_hvmc_label(opr).unwrap(), fst: Box::new(fst), snd: Box::new(snd) }
        }
        _ => unreachable!(),
      },
      Rot => {
        self.errors.push(ReadbackError::ReachedRoot);
        Term::Era
      }
      Tup => match next.slot() {
        // If we're visiting a port 0, then it is a Tup.
        0 => {
          let fst = self.read_term(self.net.enter_port(Port(node, 1)));
          let snd = self.read_term(self.net.enter_port(Port(node, 2)));
          Term::Tup { fst: Box::new(fst), snd: Box::new(snd) }
        }
        // If we're visiting a port 1 or 2, then it is a variable.
        1 | 2 => {
          self.scope.insert(node);
          Term::Var { nam: self.namegen.var_name(next) }
        }
        _ => unreachable!(),
      },
    };

    term
  }
}

enum LetInsertion {
  Ok,
  Err(Option<Tag>, Option<Name>, Option<Name>, Term),
  Todo(Option<Tag>, Option<Name>, Option<Name>, Term),
}

impl LetInsertion {
  /// Searchers the term and inserts the let body in the position bettewn where the vars it depends are defined,
  /// and where the its vars are used
  fn search_and_insert(self, term: &mut Term, free_vars: &mut IndexSet<Name>) -> (LetInsertion, bool) {
    match term.resolve_let_scope(self, free_vars) {
      (Self::Todo(tag, fst, snd, val), true) => (term.insert_let(tag, fst, snd, val, free_vars), true),
      (ctx, uses) => (ctx, uses),
    }
  }

  /// Searches all the terms and substitutes it if only one term used the ctx vars.
  /// Otherwise, returns the context with true if more then one term used the vars,
  /// or false if none.
  fn multi_search_and_insert(
    self,
    terms: &mut [&mut Term],
    free_vars: &mut IndexSet<Name>,
  ) -> (LetInsertion, bool) {
    let mut var_uses = Vec::with_capacity(terms.len());
    let mut ctx = self;
    let mut var_use;

    for term in terms.iter_mut() {
      (ctx, var_use) = term.resolve_let_scope(ctx, free_vars);
      var_uses.push(var_use);
    }

    let used_in_terms: Vec<_> =
      var_uses.into_iter().enumerate().filter_map(|(index, is_used)| is_used.then_some(index)).collect();

    match (used_in_terms.len(), ctx) {
      (1, Self::Todo(tag, fst, snd, val)) => {
        (terms[used_in_terms[0]].insert_let(tag, fst, snd, val, free_vars), true)
      }
      (0, ctx) => (ctx, false),
      (_, ctx) => (ctx, true),
    }
  }
}

impl Term {
  fn insert_let(
    &mut self,
    tag: Option<Tag>,
    fst: Option<Name>,
    snd: Option<Name>,
    val: Term,
    free_vars: &mut IndexSet<Name>,
  ) -> LetInsertion {
    // If all the vars it depends on were found, we update the term with the Let
    if free_vars.is_empty() {
      let nxt = Box::new(std::mem::replace(self, Term::Era));

      *self = match tag {
        None => Term::Let { pat: Pattern::Tup(fst, snd), val: Box::new(val), nxt },
        Some(tag) => Term::Dup { tag, fst, snd, val: Box::new(val), nxt },
      };

      LetInsertion::Ok
    } else {
      // Otherwise, return a failed attempt, that will pass through to the first call to `search and insert`
      LetInsertion::Err(tag, fst, snd, val)
    }
  }

  fn resolve_let_scope(&mut self, ctx: LetInsertion, free_vars: &mut IndexSet<Name>) -> (LetInsertion, bool) {
    match self {
      Term::Lam { nam: Some(nam), bod, .. } => {
        free_vars.remove(nam);
        ctx.search_and_insert(bod, free_vars)
      }

      Term::Lam { nam: None, bod, .. } => ctx.search_and_insert(bod, free_vars),

      Term::Let { pat: Pattern::Var(_), .. } => unreachable!(),

      Term::Let { pat: Pattern::Tup(fst, snd), val, nxt } | Term::Dup { fst, snd, val, nxt, .. } => {
        let (ctx, val_use) = val.resolve_let_scope(ctx, free_vars);

        fst.as_ref().map(|fst| free_vars.remove(fst));
        snd.as_ref().map(|snd| free_vars.remove(snd));
        let (ctx, nxt_use) = nxt.resolve_let_scope(ctx, free_vars);

        (ctx, val_use || nxt_use)
      }

      Term::Let { .. } => todo!(),

      Term::Match { scrutinee, arms } => {
        let (mut ctx, mut val_use) = scrutinee.resolve_let_scope(ctx, free_vars);

        for (rule, term) in arms {
          if let Pattern::Num(MatchNum::Succ(Some(p))) = rule {
            free_vars.remove(p);
          }

          let (arm_ctx, arm_use) = term.resolve_let_scope(ctx, free_vars);
          val_use &= arm_use;
          ctx = arm_ctx;
        }

        (ctx, val_use)
      }

      Term::Var { nam } => {
        if let LetInsertion::Todo(tag, fst, snd, val) = ctx {
          let is_fst = fst.as_ref().is_some_and(|fst| fst == nam);
          let is_snd = snd.as_ref().is_some_and(|snd| snd == nam);

          (LetInsertion::Todo(tag, fst, snd, val), is_fst || is_snd)
        } else {
          (ctx, false)
        }
      }

      Term::Chn { bod, .. } => ctx.search_and_insert(bod, free_vars),

      Term::App { fun: fst, arg: snd, .. }
      | Term::Tup { fst, snd }
      | Term::Sup { fst, snd, .. }
      | Term::Opx { fst, snd, .. } => ctx.multi_search_and_insert(&mut [fst, snd], free_vars),

      Term::Lnk { .. } | Term::Num { .. } | Term::Ref { .. } | Term::Era => (ctx, false),
    }
  }
}

#[derive(Default)]
struct Scope {
  vec: Vec<NodeId>,
  set: HashSet<NodeId>,
}

impl Scope {
  fn insert(&mut self, node: NodeId) {
    if !self.set.contains(&node) {
      self.set.insert(node);
      self.vec.push(node);
    }
  }
}

#[derive(Default)]
struct NameGen {
  var_port_to_id: HashMap<Port, Val>,
  id_counter: Val,
}

impl NameGen {
  // Given a port, returns its name, or assigns one if it wasn't named yet.
  fn var_name(&mut self, var_port: Port) -> Name {
    let id = self.var_port_to_id.entry(var_port).or_insert_with(|| {
      let id = self.id_counter;
      self.id_counter += 1;
      id
    });

    var_id_to_name(*id)
  }

  fn decl_name(&mut self, net: &INet, var_port: Port) -> Option<Name> {
    // If port is linked to an erase node, return an unused variable
    let var_use = net.enter_port(var_port);
    let var_kind = net.node(var_use.node()).kind;
    if let Era = var_kind { None } else { Some(self.var_name(var_port)) }
  }
}

impl Op {
  pub fn from_hvmc_label(value: Loc) -> Option<Op> {
    match value {
      0x0 => Some(Op::ADD),
      0x1 => Some(Op::SUB),
      0x2 => Some(Op::MUL),
      0x3 => Some(Op::DIV),
      0x4 => Some(Op::MOD),
      0x5 => Some(Op::EQ),
      0x6 => Some(Op::NE),
      0x7 => Some(Op::LT),
      0x8 => Some(Op::GT),
      0x9 => Some(Op::LTE),
      0xa => Some(Op::GTE),
      0xb => Some(Op::AND),
      0xc => Some(Op::OR),
      0xd => Some(Op::XOR),
      0xe => Some(Op::LSH),
      0xf => Some(Op::RSH),
      0x10 => Some(Op::NOT),
      _ => None,
    }
  }
}

impl Book {
  pub fn is_generated_def(&self, def_id: DefId) -> bool {
    self.def_names.name(&def_id).map_or(false, |Name(name)| name.contains('$'))
  }
}

impl Term {
  fn fix_names(&mut self, id_counter: &mut Val, book: &Book) {
    fn fix_name(nam: &mut Option<Name>, id_counter: &mut Val, bod: &mut Term) {
      if let Some(nam) = nam {
        let name = var_id_to_name(*id_counter);
        *id_counter += 1;
        bod.subst(nam, &Term::Var { nam: name.clone() });
        *nam = name;
      }
    }

    match self {
      Term::Lam { nam, bod, .. } => {
        fix_name(nam, id_counter, bod);
        bod.fix_names(id_counter, book);
      }
      Term::Ref { def_id } => {
        if book.is_generated_def(*def_id) {
          let def = book.defs.get(def_id).unwrap();
          def.assert_no_pattern_matching_rules();
          let mut term = def.rules[0].body.clone();
          term.fix_names(id_counter, book);
          *self = term
        }
      }
      Term::Dup { fst, snd, val, nxt, .. } => {
        val.fix_names(id_counter, book);
        fix_name(fst, id_counter, nxt);
        fix_name(snd, id_counter, nxt);
        nxt.fix_names(id_counter, book);
      }
      Term::Chn { bod, .. } => bod.fix_names(id_counter, book),
      Term::App { fun: fst, arg: snd, .. }
      | Term::Sup { fst, snd, .. }
      | Term::Tup { fst, snd }
      | Term::Opx { op: _, fst, snd } => {
        fst.fix_names(id_counter, book);
        snd.fix_names(id_counter, book);
      }
      Term::Match { scrutinee, arms } => {
        scrutinee.fix_names(id_counter, book);

        for (rule, term) in arms {
          if let Pattern::Num(MatchNum::Succ(nam)) = rule {
            fix_name(nam, id_counter, term);
          }

          term.fix_names(id_counter, book)
        }
      }
      Term::Let { .. } => unreachable!(),
      Term::Var { .. } | Term::Lnk { .. } | Term::Num { .. } | Term::Era => {}
    }
  }
}
