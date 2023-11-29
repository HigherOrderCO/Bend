use super::{var_id_to_name, Book, DefId, LetPat, MatchNum, Name, Op, RulePat, Term, Val};
use crate::net::{INet, NodeId, NodeKind::*, Port, SlotId, ROOT};
use hvmc::run::Loc;
use indexmap::IndexSet;
use std::collections::{HashMap, HashSet};

// TODO: Display scopeless lambdas as such
/// Converts an Interaction-INet to a Lambda Calculus term, resolvind Dups and Sups where possible.
pub fn net_to_term_non_linear(net: &INet, book: &Book, labels_to_tag: &HashMap<u32, Name>) -> (Term, bool) {
  /// Reads a term recursively by starting at root node.
  /// Returns the term and whether it's a valid readback.
  fn reader(
    net: &INet,
    next: Port,
    namegen: &mut NameGen,
    dup_scope: &mut HashMap<u32, Vec<SlotId>>,
    tup_scope: &mut Scope,
    labels_to_tag: &HashMap<u32, Name>,
    book: &Book,
  ) -> (Term, bool) {
    let node = next.node();

    match net.node(node).kind {
      // If we're visiting a set...
      Era => {
        // Only the main port actually exists in an ERA, the auxes are just an artifact of this representation.
        let valid = next.slot() == 0;
        (Term::Era, valid)
      }
      // If we're visiting a con node...
      Con => match next.slot() {
        // If we're visiting a port 0, then it is a lambda.
        0 => {
          let nam = namegen.decl_name(net, Port(node, 1));
          let prt = net.enter_port(Port(node, 2));
          let (bod, valid) = reader(net, prt, namegen, dup_scope, tup_scope, labels_to_tag, book);
          (Term::Lam { nam, bod: Box::new(bod) }, valid)
        }
        // If we're visiting a port 1, then it is a variable.
        1 => (Term::Var { nam: namegen.var_name(next) }, true),
        // If we're visiting a port 2, then it is an application.
        2 => {
          let prt = net.enter_port(Port(node, 0));
          let (fun, fun_valid) = reader(net, prt, namegen, dup_scope, tup_scope, labels_to_tag, book);
          let prt = net.enter_port(Port(node, 1));
          let (arg, arg_valid) = reader(net, prt, namegen, dup_scope, tup_scope, labels_to_tag, book);
          let valid = fun_valid && arg_valid;
          (Term::App { fun: Box::new(fun), arg: Box::new(arg) }, valid)
        }
        _ => unreachable!(),
      },
      Mat => match next.slot() {
        2 => {
          // Read the matched expression
          let cond_port = net.enter_port(Port(node, 0));
          let (cond_term, cond_valid) = reader(net, cond_port, namegen, dup_scope, tup_scope, labels_to_tag, book);

          // Read the pattern matching node
          let sel_node = net.enter_port(Port(node, 1)).node();

          // We expect the pattern matching node to be a CON
          let sel_kind = net.node(sel_node).kind;
          if sel_kind != Con {
            // TODO: Is there any case where we expect a different node type here on readback?
            return (Term::new_native_match(cond_term, Term::Era, None, Term::Era), false);
          }

          let zero_port = net.enter_port(Port(sel_node, 1));
          let (zero_term, zero_valid) = reader(net, zero_port, namegen, dup_scope, tup_scope, labels_to_tag, book);
          let succ_port = net.enter_port(Port(sel_node, 2));
          let (succ_term, succ_valid) = reader(net, succ_port, namegen, dup_scope, tup_scope, labels_to_tag, book);

          let valid = cond_valid && zero_valid && succ_valid;

          let Term::Lam { nam, bod } = succ_term else { unreachable!() };

          let term = Term::new_native_match(cond_term, zero_term, nam, *bod);
          (term, valid)
        }
        _ => unreachable!(),
      },
      Ref { def_id } => {
        if book.is_generated_def(def_id) {
          let def = book.defs.get(&def_id).unwrap();
          def.assert_no_pattern_matching_rules();
          let mut term = def.rules[0].body.clone();
          term.fix_names(&mut namegen.id_counter, book);

          (term, true)
        } else {
          (Term::Ref { def_id }, true)
        }
      }
      // If we're visiting a fan node...
      Dup { lab } => match next.slot() {
        // If we're visiting a port 0, then it is a pair.
        0 => {
          let stack = dup_scope.entry(lab).or_default();
          if let Some(slot) = stack.pop() {
            // Since we had a paired Dup in the path to this Sup,
            // we "decay" the superposition according to the original direction we came from the Dup.
            let chosen = net.enter_port(Port(node, slot));
            let (val, valid) = reader(net, chosen, namegen, dup_scope, tup_scope, labels_to_tag, book);
            dup_scope.get_mut(&lab).unwrap().push(slot);
            (val, valid)
          } else {
            // If no Dup with same label in the path, we can't resolve the Sup, so keep it as a term.
            let fst = net.enter_port(Port(node, 1));
            let snd = net.enter_port(Port(node, 2));
            let (fst, fst_valid) = reader(net, fst, namegen, dup_scope, tup_scope, labels_to_tag, book);
            let (snd, snd_valid) = reader(net, snd, namegen, dup_scope, tup_scope, labels_to_tag, book);
            let valid = fst_valid && snd_valid;
            let tag = labels_to_tag.get(&lab).cloned().unwrap_or_else(|| Name::new("auto"));
            (Term::Sup { tag, fst: Box::new(fst), snd: Box::new(snd) }, valid)
          }
        }
        // If we're visiting a port 1 or 2, then it is a variable.
        // Also, that means we found a dup, so we store it to read later.
        1 | 2 => {
          let body = net.enter_port(Port(node, 0));
          dup_scope.entry(lab).or_default().push(next.slot());
          let (body, valid) = reader(net, body, namegen, dup_scope, tup_scope, labels_to_tag, book);
          dup_scope.entry(lab).or_default().pop().unwrap();
          (body, valid)
        }
        _ => unreachable!(),
      },
      Num { val } => (Term::Num { val }, true),
      Op2 { opr } => match next.slot() {
        2 => {
          let op_port = net.enter_port(Port(node, 0));
          let (fst, fst_valid) = reader(net, op_port, namegen, dup_scope, tup_scope, labels_to_tag, book);
          let arg_port = net.enter_port(Port(node, 1));
          let (snd, snd_valid) = reader(net, arg_port, namegen, dup_scope, tup_scope, labels_to_tag, book);
          let valid = fst_valid && snd_valid;

          let term =
            Term::Opx { op: Op::from_hvmc_label(opr).unwrap(), fst: Box::new(fst), snd: Box::new(snd) };

          (term, valid)
        }
        _ => unreachable!(),
      },
      Rot => (Term::Era, false),
      Tup => match next.slot() {
        // If we're visiting a port 0, then it is a Tup.
        0 => {
          let fst_port = net.enter_port(Port(node, 1));
          let (fst, fst_valid) = reader(net, fst_port, namegen, dup_scope, tup_scope, labels_to_tag, book);
          let snd_port = net.enter_port(Port(node, 2));
          let (snd, snd_valid) = reader(net, snd_port, namegen, dup_scope, tup_scope, labels_to_tag, book);
          let valid = fst_valid && snd_valid;
          (Term::Tup { fst: Box::new(fst), snd: Box::new(snd) }, valid)
        }
        // If we're visiting a port 1 or 2, then it is a variable.
        // Also, that means we found a let, so we store it to read later.
        1 | 2 => {
          tup_scope.insert(node);
          (Term::Var { nam: namegen.var_name(next) }, true)
        }
        _ => unreachable!(),
      },
    }
  }
  // A hashmap linking ports to binder names. Those ports have names:
  // Port 1 of a con node (λ), ports 1 and 2 of a fan node (let).
  let mut namegen = NameGen::default();

  let mut dup_scope = HashMap::new();
  let mut tup_scope = Scope::default();

  // Reads the main term from the net
  let (mut main, mut valid) =
    reader(net, net.enter_port(ROOT), &mut namegen, &mut dup_scope, &mut tup_scope, labels_to_tag, book);

  // Read all the let bodies.
  while let Some(tup) = tup_scope.vec.pop() {
    let val = net.enter_port(Port(tup, 0));
    let (val, val_valid) = reader(net, val, &mut namegen, &mut dup_scope, &mut tup_scope, labels_to_tag, book);
    let fst = namegen.decl_name(net, Port(tup, 1));
    let snd = namegen.decl_name(net, Port(tup, 2));

    let mut free_vars = val.free_vars().into_keys().collect();

    let let_ctx = LetInsertion::Todo(fst, snd, val);

    match let_ctx.search_and_insert(&mut main, &mut free_vars).0 {
      LetInsertion::Err(fst, snd, val) => {
        main = Term::Let { pat: LetPat::Tup(fst, snd), val: Box::new(val), nxt: Box::new(main) }
      }
      LetInsertion::Ok => {}
      _ => unreachable!(),
    }

    valid = valid && val_valid;
  }

  (main, valid)
}

enum LetInsertion {
  Ok,
  Err(Option<Name>, Option<Name>, Term),
  Todo(Option<Name>, Option<Name>, Term),
}

impl LetInsertion {
  /// Searchers the term and inserts the let body in the position bettewn where the vars it depends are defined,
  /// and where the its vars are used
  fn search_and_insert(self, term: &mut Term, free_vars: &mut IndexSet<Name>) -> (LetInsertion, bool) {
    match term.resolve_let_scope(self, free_vars) {
      (Self::Todo(fst, snd, val), true) => (term.insert_let(fst, snd, val, free_vars), true),
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
      (1, Self::Todo(fst, snd, val)) => (terms[used_in_terms[0]].insert_let(fst, snd, val, free_vars), true),
      (0, ctx) => (ctx, false),
      (_, ctx) => (ctx, true),
    }
  }
}

impl Term {
  fn insert_let(
    &mut self,
    fst: Option<Name>,
    snd: Option<Name>,
    val: Term,
    free_vars: &mut IndexSet<Name>,
  ) -> LetInsertion {
    // If all the vars it depends on were found, we update the term with the Let
    if free_vars.is_empty() {
      let nxt = Box::new(std::mem::replace(self, Term::Era));

      *self = Term::Let { pat: LetPat::Tup(fst, snd), val: Box::new(val), nxt };
      LetInsertion::Ok
    } else {
      // Otherwise, return a failed attempt, that will pass through to the first call to `search and insert`
      LetInsertion::Err(fst, snd, val)
    }
  }

  fn resolve_let_scope(&mut self, ctx: LetInsertion, free_vars: &mut IndexSet<Name>) -> (LetInsertion, bool) {
    match self {
      Term::Lam { nam: Some(nam), bod } => {
        free_vars.remove(nam);
        ctx.search_and_insert(bod, free_vars)
      }

      Term::Lam { nam: None, bod } => ctx.search_and_insert(bod, free_vars),

      Term::Let { pat: LetPat::Var(_), .. } => unreachable!(),

      Term::Let { pat: LetPat::Tup(fst, snd), val, nxt } | Term::Dup { fst, snd, val, nxt, .. } => {
        let (ctx, val_use) = val.resolve_let_scope(ctx, free_vars);

        fst.as_ref().map(|fst| free_vars.remove(fst));
        snd.as_ref().map(|snd| free_vars.remove(snd));
        let (ctx, nxt_use) = nxt.resolve_let_scope(ctx, free_vars);

        (ctx, val_use || nxt_use)
      }

      Term::Match { scrutinee, arms } => {
        let (mut ctx, mut val_use) = scrutinee.resolve_let_scope(ctx, free_vars);

        for (rule, term) in arms {
          if let RulePat::Num(MatchNum::Succ(Some(p))) = rule {
            free_vars.remove(p);
          }

          let (arm_ctx, arm_use) = term.resolve_let_scope(ctx, free_vars);
          val_use &= arm_use;
          ctx = arm_ctx;
        }

        (ctx, val_use)
      }

      Term::Var { nam } => {
        if let LetInsertion::Todo(fst, snd, val) = ctx {
          let is_fst = fst.as_ref().map_or(false, |fst| fst == nam);
          let is_snd = snd.as_ref().map_or(false, |snd| snd == nam);

          (LetInsertion::Todo(fst, snd, val), is_fst || is_snd)
        } else {
          (ctx, false)
        }
      }

      Term::Chn { bod, .. } => ctx.search_and_insert(bod, free_vars),

      Term::App { fun, arg } => ctx.multi_search_and_insert(&mut [fun, arg], free_vars),

      Term::Tup { fst, snd } | Term::Sup { fst, snd, .. } | Term::Opx { fst, snd, .. } => {
        ctx.multi_search_and_insert(&mut [fst, snd], free_vars)
      }

      Term::Lnk { .. } | Term::Num { .. } | Term::Ref { .. } | Term::Era => (ctx, false),
    }
  }
}

/// Converts an Interaction-INet to an Interaction Calculus term.
pub fn net_to_term_linear(net: &INet, book: &Book, labels_to_tag: &HashMap<u32, Name>) -> (Term, bool) {
  /// Reads a term recursively by starting at root node.
  /// Returns the term and whether it's a valid readback.
  fn reader(
    net: &INet,
    next: Port,
    namegen: &mut NameGen,
    dup_scope: &mut Scope,
    tup_scope: &mut Scope,
    seen: &mut HashSet<Port>,
    labels_to_tag: &HashMap<u32, Name>,
    book: &Book,
  ) -> (Term, bool) {
    if seen.contains(&next) {
      return (Term::Var { nam: Name::new("...") }, false);
    }
    seen.insert(next);

    let node = next.node();

    match net.node(node).kind {
      // If we're visiting a set...
      Era => {
        // Only the main port actually exists in an ERA, the auxes are just an artifact of this representation.
        let valid = next.slot() == 0;
        (Term::Era, valid)
      }
      // If we're visiting a con node...
      Con => match next.slot() {
        // If we're visiting a port 0, then it is a lambda.
        0 => {
          seen.insert(Port(node, 2));
          let nam = namegen.decl_name(net, Port(node, 1));
          let prt = net.enter_port(Port(node, 2));
          let (bod, valid) = reader(net, prt, namegen, dup_scope, tup_scope, seen, labels_to_tag, book);
          (Term::Lam { nam, bod: Box::new(bod) }, valid)
        }
        // If we're visiting a port 1, then it is a variable.
        1 => (Term::Var { nam: namegen.var_name(next) }, true),
        // If we're visiting a port 2, then it is an application.
        2 => {
          seen.insert(Port(node, 0));
          seen.insert(Port(node, 1));
          let prt = net.enter_port(Port(node, 0));
          let (fun, fun_valid) = reader(net, prt, namegen, dup_scope, tup_scope, seen, labels_to_tag, book);
          let prt = net.enter_port(Port(node, 1));
          let (arg, arg_valid) = reader(net, prt, namegen, dup_scope, tup_scope, seen, labels_to_tag, book);
          let valid = fun_valid && arg_valid;
          (Term::App { fun: Box::new(fun), arg: Box::new(arg) }, valid)
        }
        _ => unreachable!(),
      },
      Mat => match next.slot() {
        2 => {
          // Read the matched expression
          seen.insert(Port(node, 0));
          seen.insert(Port(node, 1));
          let cond_port = net.enter_port(Port(node, 0));
          let (cond_term, cond_valid) = reader(net, cond_port, namegen, dup_scope, tup_scope, seen, labels_to_tag, book);

          // Read the pattern matching node
          let sel_node = net.enter_port(Port(node, 1)).node();
          seen.insert(Port(sel_node, 0));
          seen.insert(Port(sel_node, 1));
          seen.insert(Port(sel_node, 2));

          // We expect the pattern matching node to be a CON
          let sel_kind = net.node(sel_node).kind;
          if sel_kind != Con {
            // TODO: Is there any case where we expect a different node type here on readback?
            return (Term::new_native_match(cond_term, Term::Era, None, Term::Era), false);
          }

          let zero_port = net.enter_port(Port(sel_node, 1));
          let (zero_term, zero_valid) = reader(net, zero_port, namegen, dup_scope, tup_scope, seen, labels_to_tag, book);
          let succ_port = net.enter_port(Port(sel_node, 2));
          let (succ_term, succ_valid) = reader(net, succ_port, namegen, dup_scope, tup_scope, seen, labels_to_tag, book);

          let valid = cond_valid && zero_valid && succ_valid;

          let Term::Lam { nam, bod } = succ_term else { unreachable!() };

          let term = Term::new_native_match(cond_term, zero_term, nam, *bod);
          (term, valid)
        }
        _ => unreachable!(),
      },
      Ref { def_id } => {
        if book.is_generated_def(def_id) {
          let def = book.defs.get(&def_id).unwrap();
          def.assert_no_pattern_matching_rules();
          let mut term = def.rules[0].body.clone();
          term.fix_names(&mut namegen.id_counter, book);

          (term, true)
        } else {
          (Term::Ref { def_id }, true)
        }
      }
      // If we're visiting a fan node...
      Dup { lab } => match next.slot() {
        // If we're visiting a port 0, then it is a pair.
        0 => {
          seen.insert(Port(node, 1));
          seen.insert(Port(node, 2));
          let fst_port = net.enter_port(Port(node, 1));
          let (fst, fst_valid) = reader(net, fst_port, namegen, dup_scope, tup_scope, seen, labels_to_tag, book);
          let snd_port = net.enter_port(Port(node, 2));
          let (snd, snd_valid) = reader(net, snd_port, namegen, dup_scope, tup_scope, seen, labels_to_tag, book);
          let valid = fst_valid && snd_valid;
          let tag = labels_to_tag.get(&lab).cloned().unwrap_or_else(|| Name::new("auto"));
          (Term::Sup { tag, fst: Box::new(fst), snd: Box::new(snd) }, valid)
        }
        // If we're visiting a port 1 or 2, then it is a variable.
        // Also, that means we found a dup, so we store it to read later.
        1 | 2 => {
          dup_scope.insert(node);
          (Term::Var { nam: namegen.var_name(next) }, true)
        }
        _ => unreachable!(),
      },
      Num { val } => (Term::Num { val }, true),
      Op2 { .. } => todo!(),
      // If we're revisiting the root node something went wrong with this net
      Rot => (Term::Era, false),
      Tup => match next.slot() {
        // If we're visiting a port 0, then it is a Tup.
        0 => {
          seen.insert(Port(node, 1));
          seen.insert(Port(node, 2));
          let fst_port = net.enter_port(Port(node, 1));
          let (fst, fst_valid) = reader(net, fst_port, namegen, dup_scope, tup_scope, seen, labels_to_tag, book);
          let snd_port = net.enter_port(Port(node, 2));
          let (snd, snd_valid) = reader(net, snd_port, namegen, dup_scope, tup_scope, seen, labels_to_tag, book);
          let valid = fst_valid && snd_valid;
          (Term::Tup { fst: Box::new(fst), snd: Box::new(snd) }, valid)
        }
        // If we're visiting a port 1 or 2, then it is a variable.
        // Also, that means we found a let, so we store it to read later.
        1 | 2 => {
          tup_scope.insert(node);
          (Term::Var { nam: namegen.var_name(next) }, true)
        }
        _ => unreachable!(),
      },
    }
  }

  // A hashmap linking ports to binder names. Those ports have names:
  // Port 1 of a con node (λ), ports 1 and 2 of a fan node (let).
  let mut namegen = NameGen::default();

  // Dup aren't scoped. We find them when we read one of the variables
  // introduced by them. Thus, we must store the dups we find to read later.
  // We have a vec for .pop(). and a set to avoid storing duplicates.
  let mut dup_scope = Scope::default();
  let mut tup_scope = Scope::default();
  let mut seen = HashSet::new();

  // Reads the main term from the net
  let (mut main, mut valid) =
    reader(net, net.enter_port(ROOT), &mut namegen, &mut dup_scope, &mut tup_scope, &mut seen, labels_to_tag, book);

  // Read all the dup bodies.
  while let Some(dup) = dup_scope.vec.pop() {
    seen.insert(Port(dup, 0));
    let val = net.enter_port(Port(dup, 0));
    let (val, val_valid) = reader(net, val, &mut namegen, &mut dup_scope, &mut tup_scope, &mut seen, labels_to_tag, book);
    let fst = namegen.decl_name(net, Port(dup, 1));
    let snd = namegen.decl_name(net, Port(dup, 2));
    main = Term::Dup { tag: None, fst, snd, val: Box::new(val), nxt: Box::new(main) };
    valid = valid && val_valid;
  }

  // Read all the let bodies.
  while let Some(tup) = tup_scope.vec.pop() {
    seen.insert(Port(tup, 0));
    let val = net.enter_port(Port(tup, 0));
    let (val, val_valid) = reader(net, val, &mut namegen, &mut dup_scope, &mut tup_scope, &mut seen, labels_to_tag, book);
    let fst = namegen.decl_name(net, Port(tup, 1));
    let snd = namegen.decl_name(net, Port(tup, 2));
    main = Term::Let { pat: LetPat::Tup(fst, snd), val: Box::new(val), nxt: Box::new(main) };
    valid = valid && val_valid;
  }

  // Check if the readback didn't leave any unread nodes (for example reading var from a lam but never reading the lam itself)
  for &decl_port in namegen.var_port_to_id.keys() {
    for check_slot in 0 .. 3 {
      let check_port = Port(decl_port.node(), check_slot);
      let other_node = net.enter_port(check_port).node();
      if !seen.contains(&check_port) && net.node(other_node).kind != Era {
        valid = false;
      }
    }
  }

  (main, valid)
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
      Term::Lam { nam, bod } => {
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
      Term::Chn { nam: _, bod } => bod.fix_names(id_counter, book),
      Term::App { fun: fst, arg: snd }
      | Term::Sup { fst, snd, .. }
      | Term::Tup { fst, snd }
      | Term::Opx { op: _, fst, snd } => {
        fst.fix_names(id_counter, book);
        snd.fix_names(id_counter, book);
      }
      Term::Match { scrutinee, arms } => {
        scrutinee.fix_names(id_counter, book);

        for (rule, term) in arms {
          if let RulePat::Num(MatchNum::Succ(nam)) = rule {
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
