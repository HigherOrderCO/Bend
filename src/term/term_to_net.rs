use super::{Book, DefId, DefNames, LetPat, Name, Op, Term};
use crate::net::{INet, NodeId, NodeKind::*, Port, LABEL_MASK, ROOT};
use hvmc::{
  ast::{name_to_val, val_to_name},
  run::Val,
};
use std::collections::HashMap;

pub fn book_to_nets(
  book: &Book,
  main: DefId,
) -> anyhow::Result<(HashMap<String, INet>, HashMap<DefId, Val>)> {
  let mut nets = HashMap::new();
  let mut id_to_hvmc_name = HashMap::new();

  for def in book.defs.values() {
    for rule in def.rules.iter() {
      let net = term_to_compat_net(&rule.body)?;
      let name = if def.def_id == main {
        DefNames::ENTRY_POINT.to_string()
      } else {
        def_id_to_hvmc_name(book, def.def_id, &nets)
      };

      id_to_hvmc_name.insert(def.def_id, name_to_val(&name));
      nets.insert(name, net);
    }
  }

  Ok((nets, id_to_hvmc_name))
}

/// Converts rules names to unique names compatible with hvm-core:
///   If the rule is compiler-generated: Convert the DefId value into a new name
///   If not: Truncates the rule name into 4 chars
/// Them checks if the given hashmap already contains the resulted name,
/// if it does, falls back into converting its DefId and succeeding ones until a unique name is found.
fn def_id_to_hvmc_name(book: &Book, def_id: DefId, nets: &HashMap<String, INet>) -> String {
  fn truncate(s: &str, max_chars: usize) -> &str {
    match s.char_indices().nth(max_chars) {
      None => s,
      Some((idx, _)) => &s[.. idx],
    }
  }

  fn gen_unique_name(def_id: DefId, nets: &HashMap<String, INet>) -> String {
    let name = val_to_name(def_id.to_internal());
    if nets.contains_key(&name) { gen_unique_name(DefId(def_id.0 + 1), nets) } else { name }
  }

  if book.is_generated_def(def_id) {
    gen_unique_name(def_id, nets)
  } else {
    let Name(name) = book.def_names.name(&def_id).unwrap();
    let name = truncate(name, 4);
    if !(nets.contains_key(name) || name.eq(DefNames::ENTRY_POINT)) {
      name.to_owned()
    } else {
      gen_unique_name(def_id, nets)
    }
  }
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
      let app = inet.new_node(Con);
      let fun = encode_term(inet, fun, Port(app, 0), scope, vars, global_vars, dups)?;
      link_local(inet, Port(app, 0), fun);
      let arg = encode_term(inet, arg, Port(app, 1), scope, vars, global_vars, dups)?;
      link_local(inet, Port(app, 1), arg);
      Ok(Some(Port(app, 2)))
    }
    // core: & cond ~ ? (zero succ) ret
    Term::Match { cond, zero, succ } => {
      let if_ = inet.new_node(Mat);

      let cond = encode_term(inet, cond, Port(if_, 0), scope, vars, global_vars, dups)?;
      link_local(inet, Port(if_, 0), cond);

      let sel = inet.new_node(Con);
      inet.link(Port(sel, 0), Port(if_, 1));

      let zero = encode_term(inet, zero, Port(sel, 1), scope, vars, global_vars, dups)?;
      link_local(inet, Port(sel, 1), zero);

      let succ = encode_term(inet, succ, Port(sel, 2), scope, vars, global_vars, dups)?;
      link_local(inet, Port(sel, 2), succ);

      Ok(Some(Port(if_, 2)))
    }
    // A dup becomes a dup node too. Ports:
    // - 0: points to the value projected.
    // - 1: points to the occurrence of the first variable.
    // - 2: points to the occurrence of the second variable.
    // core: & val ~ {lab fst snd} (val not necessarily main port)
    Term::Dup { fst, snd, val, nxt } => {
      let dup = inet.new_node(Dup { lab: u8::try_from(*dups).unwrap() });
      *dups += 1;
      let val = encode_term(inet, val, Port(dup, 0), scope, vars, global_vars, dups)?;
      link_local(inet, Port(dup, 0), val);

      push_scope(fst, Port(dup, 1), scope, vars);
      push_scope(snd, Port(dup, 2), scope, vars);
      let nxt = encode_term(inet, nxt, up, scope, vars, global_vars, dups)?;
      pop_scope(snd, Port(dup, 2), inet, scope);
      pop_scope(fst, Port(dup, 1), inet, scope);

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
      inet.link(up, *declare_port);
      *use_port = Some(up);
      Ok(Some(*declare_port))
    }
    Term::Lnk { nam } => {
      global_vars.entry(nam.clone()).or_default().1 = up;
      Ok(None)
    }
    // core: @def_id
    Term::Ref { def_id } => {
      let node = inet.new_node(Ref { def_id: *def_id });
      inet.link(Port(node, 1), Port(node, 2));
      inet.link(up, Port(node, 0));
      Ok(Some(Port(node, 0)))
    }
    Term::Let { pat: LetPat::Tup(l_nam, r_nam), val, nxt } => {
      let dup = inet.new_node(Tup);

      let val = encode_term(inet, val, Port(dup, 0), scope, vars, global_vars, dups)?;
      link_local(inet, Port(dup, 0), val);

      push_scope(l_nam, Port(dup, 1), scope, vars);
      push_scope(r_nam, Port(dup, 2), scope, vars);
      let nxt = encode_term(inet, nxt, up, scope, vars, global_vars, dups)?;
      pop_scope(r_nam, Port(dup, 2), inet, scope);
      pop_scope(l_nam, Port(dup, 1), inet, scope);

      Ok(nxt)
    }
    Term::Let { .. } => unreachable!(), // Removed in earlier poss
    Term::Sup { .. } => unreachable!(), // Not supported in syntax
    Term::Era => unreachable!(),        // Not supported in syntax
    // core: #val
    Term::Num { val } => {
      debug_assert!(*val <= LABEL_MASK);
      let node = inet.new_node(Num { val: *val });
      // This representation only has nodes of arity 2, so we connect the two aux ports that are not used.
      inet.link(Port(node, 1), Port(node, 2));
      Ok(Some(Port(node, 0)))
    }
    // core: & #op ~ <fst <snd ret>>
    Term::Opx { op, fst, snd } => {
      let op_node = inet.new_node(Num { val: op.to_hvmc_label() });
      inet.link(Port(op_node, 1), Port(op_node, 2));

      let fst_node = inet.new_node(Op2);
      inet.link(Port(op_node, 0), Port(fst_node, 0));

      let fst = encode_term(inet, fst, Port(fst_node, 1), scope, vars, global_vars, dups)?;
      link_local(inet, Port(fst_node, 1), fst);

      let snd_node = inet.new_node(Op2);
      inet.link(Port(fst_node, 2), Port(snd_node, 0));

      let snd = encode_term(inet, snd, Port(snd_node, 1), scope, vars, global_vars, dups)?;
      link_local(inet, Port(snd_node, 1), snd);

      Ok(Some(Port(snd_node, 2)))
    }
    Term::Tup { fst, snd } => {
      let tup = inet.new_node(Tup);

      let fst = encode_term(inet, fst, Port(tup, 1), scope, vars, global_vars, dups)?;
      link_local(inet, Port(tup, 1), fst);

      let snd = encode_term(inet, snd, Port(tup, 2), scope, vars, global_vars, dups)?;
      link_local(inet, Port(tup, 2), snd);

      Ok(Some(Port(tup, 0)))
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
    let era = inet.new_node(Era);
    inet.link(decl_port, Port(era, 0));
    inet.link(Port(era, 1), Port(era, 2));
  }
}

fn link_local(inet: &mut INet, ptr_a: Port, ptr_b: Option<Port>) {
  if let Some(ptr_b) = ptr_b {
    inet.link(ptr_a, ptr_b);
  }
}

impl Op {
  pub fn to_hvmc_label(self) -> Val {
    match self {
      Op::ADD => 0x1,
      Op::SUB => 0x2,
      Op::MUL => 0x3,
      Op::DIV => 0x4,
      Op::MOD => 0x5,
      Op::EQ => 0x6,
      Op::NE => 0x7,
      Op::LT => 0x8,
      Op::GT => 0x9,
      Op::AND => 0xa,
      Op::OR => 0xb,
      Op::XOR => 0xc,
      Op::NOT => 0xd,
      Op::LSH => 0xe,
      Op::RSH => 0xf,
    }
  }
}
