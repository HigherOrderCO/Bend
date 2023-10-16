use super::{DefId, DefinitionBook, Name, Term};
use crate::net::{INet, NodeId, NodeKind, NodeKind::*, Port, LABEL_MASK, ROOT};
use std::collections::HashMap;

pub fn book_to_compact_nets(book: &DefinitionBook) -> anyhow::Result<Vec<(DefId, INet)>> {
  let mut nets = Vec::new();

  for rule in book.defs.iter() {
    let net = term_to_compat_net(&rule.body)?;
    nets.push((rule.def_id, net))
  }

  Ok(nets)
}

/// Converts an IC term into an IC net.
pub fn term_to_compat_net(term: &Term) -> anyhow::Result<INet> {
  let mut inet = INet::new();

  // Encodes the main term.
  let mut global_vars = HashMap::new();
  let main = encode_term(&mut inet, term, ROOT, &mut HashMap::new(), &mut vec![], &mut global_vars, &mut 0)?;

  for (decl_port, use_port) in global_vars.into_values() {
    inet.link(decl_port, use_port);
  }
  if Some(ROOT) != main {
    link_local(&mut inet, ROOT, main);
  }

  Ok(inet)
}

/// Adds a subterm connected to `up` to the `inet`.
/// `scope` has the current variable scope.
/// `vars` has the information of which ports the variables are declared and used in.
/// `global_vars` has the same information for global lambdas. Must be linked outside this function.
/// Expects variables to be affine, refs to be stored as Refs and all names to be bound.
fn encode_term(
  inet: &mut INet,
  term: &Term,
  up: Port,
  scope: &mut HashMap<Name, Vec<usize>>,
  vars: &mut Vec<(Port, Option<Port>)>,
  global_vars: &mut HashMap<Name, (Port, Port)>,
  dups: &mut NodeId,
) -> anyhow::Result<Option<Port>> {
  match term {
    // A lambda becomes to a con node. Ports:
    // - 0: points to where the lambda occurs.
    // - 1: points to the lambda variable.
    // - 2: points to the lambda body.
    // core: (var_use bod)
    Term::Lam { nam, bod } => {
      let fun = inet.new_node(Con);
      push_scope(nam, Port(fun, 1), scope, vars);
      let bod = encode_term(inet, bod, Port(fun, 2), scope, vars, global_vars, dups)?;
      pop_scope(nam, Port(fun, 1), inet, scope);
      link_local(inet, Port(fun, 2), bod);
      Ok(Some(Port(fun, 0)))
    }
    // core: (var_use bod)
    Term::Chn { nam, bod } => {
      let fun = inet.new_node(Con);
      global_vars.entry(nam.clone()).or_default().0 = Port(fun, 1);
      let bod = encode_term(inet, bod, Port(fun, 2), scope, vars, global_vars, dups)?;
      link_local(inet, Port(fun, 2), bod);
      Ok(Some(Port(fun, 0)))
    }
    // An application becomes to a con node too. Ports:
    // - 0: points to the function being applied.
    // - 1: points to the function's argument.
    // - 2: points to where the application occurs.
    // core: & fun ~ (arg ret) (fun not necessarily main port)
    Term::App { fun, arg } => {
      let app = new_node(inet, CON);
      let fun = encode_term(inet, fun, port(app, 0), scope, vars, global_vars, dups)?;
      link_local(inet, port(app, 0), fun);
      let arg = encode_term(inet, arg, port(app, 1), scope, vars, global_vars, dups)?;
      link_local(inet, port(app, 1), arg);
      Ok(Some(port(app, 2)))
    }
    // core: & cond ~ ? (zero succ) ret
    Term::Match { cond, zero, succ } => {
      let if_ = new_node(inet, MAT);

      let cond = encode_term(inet, cond, port(if_, 0), scope, vars, global_vars, dups)?;
      link_local(inet, port(if_, 0), cond);

      let sel = new_node(inet, CON);
      link(inet, port(sel, 0), port(if_, 1));

      let zero = encode_term(inet, zero, port(sel, 1), scope, vars, global_vars, dups)?;
      link_local(inet, port(sel, 1), zero);

      let succ = encode_term(inet, succ, port(sel, 2), scope, vars, global_vars, dups)?;
      link_local(inet, port(sel, 2), succ);

      Ok(Some(port(if_, 2)))
    }
    // A dup becomes a dup node too. Ports:
    // - 0: points to the value projected.
    // - 1: points to the occurrence of the first variable.
    // - 2: points to the occurrence of the second variable.
    // core: & val ~ {lab fst snd} (val not necessarily main port)
    Term::Dup { fst, snd, val, nxt } => {
      let dup = new_node(inet, DUP | *dups);
      *dups += 1;
      let val = encode_term(inet, val, port(dup, 0), scope, vars, global_vars, dups)?;
      link_local(inet, port(dup, 0), val);

      push_scope(fst, port(dup, 1), scope, vars);
      push_scope(snd, port(dup, 2), scope, vars);
      let nxt = encode_term(inet, nxt, up, scope, vars, global_vars, dups)?;
      pop_scope(snd, port(dup, 2), inet, scope);
      pop_scope(fst, port(dup, 1), inet, scope);

      Ok(nxt)
    }
    Term::Var { nam } => {
      // We assume this variable to be valid, bound and correctly scoped.
      // This pass must be done before.
      debug_assert!(
        scope.contains_key(nam),
        "Unbound variable {nam}. Expected this check to be already done"
      );
      let var_stack = scope.get(nam).unwrap();
      let crnt_var = *var_stack.last().unwrap();
      let (declare_port, use_port) = vars.get_mut(crnt_var).unwrap();
      debug_assert!(use_port.is_none(), "Variable {nam} used more than once");
      link(inet, up, *declare_port);
      *use_port = Some(up);
      Ok(Some(*declare_port))
    }
    Term::Lnk { nam } => {
      global_vars.entry(nam.clone()).or_default().1 = up;
      Ok(None)
    }
    // core: @def_id
    Term::Ref { def_id } => {
      let node = new_node(inet, REF | **def_id);
      link(inet, port(node, 1), port(node, 2));
      link(inet, up, port(node, 0));
      Ok(Some(port(node, 0)))
    }
    Term::Let { .. } => unreachable!(), // Removed in earlier poss
    Term::Sup { .. } => unreachable!(), // Not supported in syntax
    Term::Era => unreachable!(),        // Not supported in syntax
    // core: #val
    Term::Num { val } => {
      debug_assert!(*val as NodeKind <= LABEL_MASK);
      let node = new_node(inet, NUM | *val as NodeKind);
      // This representation only has nodes of arity 2, so we connect the two aux ports that are not used.
      link(inet, port(node, 1), port(node, 2));
      Ok(Some(port(node, 0)))
    }
    // core: & #op ~ <fst <snd ret>>
    Term::Opx { op, fst, snd } => {
      let op_node = new_node(inet, NUM | to_hvmc_label(*op));
      link(inet, port(op_node, 1), port(op_node, 2));

      let fst_node = new_node(inet, OP2);
      link(inet, port(op_node, 0), port(fst_node, 0));

      let fst = encode_term(inet, fst, port(fst_node, 1), scope, vars, global_vars, dups)?;
      link_local(inet, port(fst_node, 1), fst);

      let snd_node = new_node(inet, OP2);
      link(inet, port(fst_node, 2), port(snd_node, 0));

      let snd = encode_term(inet, snd, port(snd_node, 1), scope, vars, global_vars, dups)?;
      link_local(inet, port(snd_node, 1), snd);

      Ok(Some(port(snd_node, 2)))
    }
  }
}

fn push_scope(
  name: &Option<Name>,
  decl_port: Port,
  scope: &mut HashMap<Name, Vec<usize>>,
  vars: &mut Vec<(Port, Option<Port>)>,
) {
  if let Some(name) = name {
    scope.entry(name.clone()).or_default().push(vars.len());
    vars.push((decl_port, None));
  }
}

fn pop_scope(name: &Option<Name>, decl_port: Port, inet: &mut INet, scope: &mut HashMap<Name, Vec<usize>>) {
  if let Some(name) = name {
    scope.get_mut(name).unwrap().pop().unwrap();
  } else {
    let era = new_node(inet, ERA);
    link(inet, decl_port, port(era, 0));
    link(inet, port(era, 1), port(era, 2));
  }
}

fn link_local(inet: &mut INet, ptr_a: Port, ptr_b: Option<Port>) {
  if let Some(ptr_b) = ptr_b {
    link(inet, ptr_a, ptr_b);
  }
}
