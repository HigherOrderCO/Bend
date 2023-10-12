use crate::{
  ast::{var_id_to_name, DefId, Name, Term},
  net::inter_net::{
    addr, enter, kind, link, new_inet, new_node, port, slot, INet, INode, INodes, NodeId, NodeKind, Port,
    SlotId, CON, DUP, ERA, ITE, LABEL_MASK, REF, ROOT, TAG_MASK,
  },
  term::{DefinitionBook, Op},
};
use hvmc::{LNet, LTree, Val};
use std::collections::{HashMap, HashSet};

use crate::net::inter_net::{label_to_op, NUM, OP2};

pub fn readback_net(net: &LNet, book: &DefinitionBook) -> anyhow::Result<(Term, bool)> {
  /* check_lnet_valid(net)?; */
  let compat_net = core_net_to_compat(net)?;
  let readback = readback_compat(&compat_net, book);
  Ok(readback)
}

fn core_net_to_compat(lnet: &LNet) -> anyhow::Result<INet> {
  let inodes = lnet_to_inodes(lnet);
  let compat_net = inodes_to_inet(&inodes);
  Ok(compat_net)
}

fn lnet_to_inodes(lnet: &LNet) -> INodes {
  let mut inodes = vec![];
  let mut n_vars = 0;
  let net_root = if let LTree::Var { nam } = &lnet.root { nam } else { "" };

  // If we have a tree attached to the net root, convert that first
  if !matches!(&lnet.root, LTree::Var { .. }) {
    let mut root = tree_to_inodes(&lnet.root, "_".to_string(), net_root, &mut n_vars);
    inodes.append(&mut root);
  }
  // Convert all the trees forming active pairs.
  for (i, (tree1, tree2)) in lnet.rdex.iter().enumerate() {
    let tree_root = format!("a{i}");
    let mut tree1 = tree_to_inodes(tree1, tree_root.clone(), net_root, &mut n_vars);
    inodes.append(&mut tree1);
    let mut tree2 = tree_to_inodes(tree2, tree_root, net_root, &mut n_vars);
    inodes.append(&mut tree2);
  }
  inodes
}

fn tree_to_inodes(tree: &LTree, tree_root: String, net_root: &str, n_vars: &mut NodeId) -> INodes {
  fn new_var(n_vars: &mut NodeId) -> String {
    let new_var = format!("x{n_vars}");
    *n_vars += 1;
    new_var
  }

  fn process_node_subtree<'a>(
    subtree: &'a LTree,
    net_root: &str,
    subtrees: &mut Vec<(String, &'a LTree)>,
    n_vars: &mut NodeId,
  ) -> String {
    if let LTree::Var { nam } = subtree {
      if nam == net_root { "_".to_string() } else { nam.clone() }
    } else {
      let var = new_var(n_vars);
      subtrees.push((var.clone(), subtree));
      var
    }
  }

  let mut inodes = vec![];
  let mut subtrees = vec![(tree_root, tree)];
  while let Some((subtree_root, subtree)) = subtrees.pop() {
    match subtree {
      LTree::Era => {
        let var = new_var(n_vars);
        inodes.push(INode { kind: ERA, ports: [subtree_root, var.clone(), var] });
      }
      LTree::Ctr { lab, lft, rgt } => {
        let kind = if *lab == 0 { CON } else { DUP | (*lab - 1) as NodeKind };
        let lft = process_node_subtree(lft, net_root, &mut subtrees, n_vars);
        let rgt = process_node_subtree(rgt, net_root, &mut subtrees, n_vars);
        inodes.push(INode { kind, ports: [subtree_root, lft, rgt] })
      }
      LTree::Var { .. } => unreachable!(),
      LTree::Ref { nam } => {
        let kind = REF | (*DefId::from_internal(*nam) as NodeKind);
        let var = new_var(n_vars);
        inodes.push(INode { kind, ports: [subtree_root, var.clone(), var] });
      }
      LTree::Num { val } => {
        let kind = NUM | (*val as NodeKind);
        let var = new_var(n_vars);
        inodes.push(INode { kind, ports: [subtree_root, var.clone(), var] });
      }
      LTree::Op2 { lft, rgt } => {
        let kind = OP2;
        let lft = process_node_subtree(lft, net_root, &mut subtrees, n_vars);
        let rgt = process_node_subtree(rgt, net_root, &mut subtrees, n_vars);
        inodes.push(INode { kind, ports: [subtree_root, lft, rgt] })
      }
      LTree::Ite { .. } => todo!(),
    }
  }
  inodes
}

// Converts INodes to an INet by linking ports based on names.
fn inodes_to_inet(inodes: &INodes) -> INet {
  let mut inet = new_inet();
  let mut name_map = std::collections::HashMap::new();

  for inode in inodes.iter() {
    let node = new_node(&mut inet, inode.kind);
    for (j, name) in inode.ports.iter().enumerate() {
      let p = port(node, j as SlotId);
      if name == "_" {
        link(&mut inet, p, ROOT);
      } else if let Some(&q) = name_map.get(name) {
        link(&mut inet, p, q);
        name_map.remove(name);
      } else {
        name_map.insert(name.clone(), p);
      }
    }
  }

  inet
}

// TODO: Add support for global lambdas.
/// Converts an Interaction-INet node to an Interaction Calculus term.
fn readback_compat(net: &INet, book: &DefinitionBook) -> (Term, bool) {
  // Given a port, returns its name, or assigns one if it wasn't named yet.
  fn var_name(var_port: Port, var_port_to_id: &mut HashMap<Port, Val>, id_counter: &mut Val) -> Name {
    let id = var_port_to_id.entry(var_port).or_insert_with(|| {
      let id = *id_counter;
      *id_counter += 1;
      id
    });

    var_id_to_name(*id)
  }

  fn decl_name(
    net: &INet,
    var_port: Port,
    var_port_to_id: &mut HashMap<Port, Val>,
    id_counter: &mut Val,
  ) -> Option<Name> {
    // If port is linked to an erase node, return an unused variable
    if kind(net, addr(enter(net, var_port))) == ERA {
      None
    } else {
      Some(var_name(var_port, var_port_to_id, id_counter))
    }
  }

  /// Reads a term recursively by starting at root node.
  /// Returns the term and whether it's a valid readback.
  fn reader(
    net: &INet,
    next: Port,
    var_port_to_id: &mut HashMap<Port, Val>,
    id_counter: &mut Val,
    dups_vec: &mut Vec<NodeId>,
    dups_set: &mut HashSet<NodeId>,
    seen: &mut HashSet<Port>,
    book: &DefinitionBook,
  ) -> (Term, bool) {
    if seen.contains(&next) {
      return (Term::Var { nam: Name::new("...") }, false);
    }
    seen.insert(next);

    let node = addr(next);
    let kind_ = kind(net, node);
    let tag = kind_ & TAG_MASK;
    let label = kind_ & LABEL_MASK;

    match tag {
      // If we're visiting a set...
      ERA => {
        // Only the main port actually exists in an ERA, the auxes are just an artifact of this representation.
        let valid = slot(next) == 0;
        (Term::Era, valid)
      }
      // If we're visiting a con node...
      CON => match slot(next) {
        // If we're visiting a port 0, then it is a lambda.
        0 => {
          seen.insert(port(node, 2));
          let nam = decl_name(net, port(node, 1), var_port_to_id, id_counter);
          let prt = enter(net, port(node, 2));
          let (bod, valid) = reader(net, prt, var_port_to_id, id_counter, dups_vec, dups_set, seen, book);
          (Term::Lam { nam, bod: Box::new(bod) }, valid)
        }
        // If we're visiting a port 1, then it is a variable.
        1 => (Term::Var { nam: var_name(next, var_port_to_id, id_counter) }, true),
        // If we're visiting a port 2, then it is an application.
        2 => {
          seen.insert(port(node, 0));
          seen.insert(port(node, 1));
          let prt = enter(net, port(node, 0));
          let (fun, fun_valid) = reader(net, prt, var_port_to_id, id_counter, dups_vec, dups_set, seen, book);
          let prt = enter(net, port(node, 1));
          let (arg, arg_valid) = reader(net, prt, var_port_to_id, id_counter, dups_vec, dups_set, seen, book);
          let valid = fun_valid && arg_valid;
          (Term::App { fun: Box::new(fun), arg: Box::new(arg) }, valid)
        }
        _ => unreachable!(),
      },
      ITE => match slot(next) {
        2 => {
          seen.insert(port(node, 0));
          seen.insert(port(node, 1));
          let cond_port = enter(net, port(node, 0));
          let (cond_term, cond_valid) =
            reader(net, cond_port, var_port_to_id, id_counter, dups_vec, dups_set, seen, book);
          let branches_port = enter(net, port(node, 0));
          let branches_node = addr(branches_port);
          let branches_kind = kind(net, branches_node);
          if branches_kind & TAG_MASK == CON {
            seen.insert(port(branches_node, 0));
            seen.insert(port(branches_node, 1));
            seen.insert(port(branches_node, 2));
            let then_port = enter(net, port(node, 0));
            let (then_term, then_valid) =
              reader(net, then_port, var_port_to_id, id_counter, dups_vec, dups_set, seen, book);
            let else_port = enter(net, port(node, 0));
            let (else_term, else_valid) =
              reader(net, else_port, var_port_to_id, id_counter, dups_vec, dups_set, seen, book);
            let valid = cond_valid && then_valid && else_valid;
            (
              Term::If { cond: Box::new(cond_term), then: Box::new(then_term), els_: Box::new(else_term) },
              valid,
            )
          } else {
            // TODO: Is there any case where we expect a different node type here on readback
            (
              Term::If { cond: Box::new(cond_term), then: Box::new(Term::Era), els_: Box::new(Term::Era) },
              false,
            )
          }
        }
        _ => unreachable!(),
      },
      REF => {
        let def_id = DefId(label);
        if book.is_generated_rule(def_id) {
          let def = &book.defs[def_id.0 as usize];
          assert!(def.rules.len() == 1);

          let mut term = def.rules[0].body.clone();
          term.fix_names(id_counter, book);

          (term, true)
        } else {
          (Term::Ref { def_id }, true)
        }
      }
      // If we're visiting a fan node...
      DUP => match slot(next) {
        // If we're visiting a port 0, then it is a pair.
        0 => {
          seen.insert(port(node, 1));
          seen.insert(port(node, 2));
          let prt = enter(net, port(node, 1));
          let (fst, fst_valid) = reader(net, prt, var_port_to_id, id_counter, dups_vec, dups_set, seen, book);
          let prt = enter(net, port(node, 2));
          let (snd, snd_valid) = reader(net, prt, var_port_to_id, id_counter, dups_vec, dups_set, seen, book);
          let valid = fst_valid && snd_valid;
          (Term::Sup { fst: Box::new(fst), snd: Box::new(snd) }, valid)
        }
        // If we're visiting a port 1 or 2, then it is a variable.
        // Also, that means we found a dup, so we store it to read later.
        1 | 2 => {
          if !dups_set.contains(&node) {
            dups_set.insert(node);
            dups_vec.push(node);
          } else {
            // Second time we find, it has to be the other dup variable.
          }
          (Term::Var { nam: var_name(next, var_port_to_id, id_counter) }, true)
        }
        _ => unreachable!(),
      },
      NUM => (Term::Num { val: label as u32 }, true),
      OP2 => match slot(next) {
        2 => {
          seen.insert(port(node, 0));
          seen.insert(port(node, 1));
          let op_port = enter(net, port(node, 0));
          let (op_term, op_valid) =
            reader(net, op_port, var_port_to_id, id_counter, dups_vec, dups_set, seen, book);
          let arg_port = enter(net, port(node, 1));
          let (arg_term, fst_valid) =
            reader(net, arg_port, var_port_to_id, id_counter, dups_vec, dups_set, seen, book);
          let valid = op_valid && fst_valid;
          match op_term {
            Term::Num { val } => {
              let (val, op) = split_num_with_op(val);
              if let Some(op) = op {
                // This is Num + Op in the same value
                (Term::Opx { op, fst: Box::new(Term::Num { val }), snd: Box::new(arg_term) }, valid)
              } else {
                // This is just Op as value
                (
                  Term::Opx {
                    op: label_to_op(val).unwrap(),
                    fst: Box::new(arg_term),
                    snd: Box::new(Term::Era),
                  },
                  valid,
                )
              }
            }
            Term::Opx { op, fst, snd: _ } => (Term::Opx { op, fst, snd: Box::new(arg_term) }, valid),
            // TODO: Actually unreachable?
            _ => unreachable!(),
          }
        }
        _ => unreachable!(),
      },
      _ => unreachable!(),
    }
  }

  fn split_num_with_op(num: Val) -> (Val, Option<Op>) {
    let op = label_to_op(num >> 24);
    let num = num & ((1 << 24) - 1);
    (num, op)
  }

  // A hashmap linking ports to binder names. Those ports have names:
  // Port 1 of a con node (λ), ports 1 and 2 of a fan node (let).
  let mut var_port_to_id = HashMap::new();
  let id_counter = &mut 0;

  // Dup aren't scoped. We find them when we read one of the variables
  // introduced by them. Thus, we must store the dups we find to read later.
  // We have a vec for .pop(). and a set to avoid storing duplicates.
  let mut dups_vec = Vec::new();
  let mut dups_set = HashSet::new();
  let mut seen = HashSet::new();

  // Reads the main term from the net
  let (mut main, mut valid) = reader(
    net,
    enter(net, ROOT),
    &mut var_port_to_id,
    id_counter,
    &mut dups_vec,
    &mut dups_set,
    &mut seen,
    book,
  );

  // Read all the dup bodies.
  while let Some(dup) = dups_vec.pop() {
    seen.insert(port(dup, 0));
    let val = enter(net, port(dup, 0));
    let (val, val_valid) =
      reader(net, val, &mut var_port_to_id, id_counter, &mut dups_vec, &mut dups_set, &mut seen, book);
    let fst = decl_name(net, port(dup, 1), &mut var_port_to_id, id_counter);
    let snd = decl_name(net, port(dup, 2), &mut var_port_to_id, id_counter);
    main = Term::Dup { fst, snd, val: Box::new(val), nxt: Box::new(main) };
    valid = valid && val_valid;
  }

  // Check if the readback didn't leave any unread nodes (for example reading var from a lam but never reading the lam itself)
  for &decl_port in var_port_to_id.keys() {
    for check_slot in 0 .. 3 {
      let check_port = port(addr(decl_port), check_slot);
      let other_node = addr(enter(net, check_port));
      if !seen.contains(&check_port) && kind(net, other_node) != ERA {
        valid = false;
      }
    }
  }

  (main, valid)
}

impl DefinitionBook {
  pub fn is_generated_rule(&self, def_id: DefId) -> bool {
    self.def_names.name(&def_id).map_or(false, |Name(name)| name.contains('$'))
  }
}

impl Term {
  fn fix_names(&mut self, id_counter: &mut Val, book: &DefinitionBook) {
    match self {
      Term::Lam { nam: Some(n), bod } => {
        let name = var_id_to_name(*id_counter);
        *id_counter += 1;

        bod.subst(n, &Term::Var { nam: name.clone() });
        *n = name;

        bod.fix_names(id_counter, book);
      }
      Term::Lam { nam: None, bod } => bod.fix_names(id_counter, book),
      Term::Var { .. } => {}
      Term::Chn { nam: _, bod } => bod.fix_names(id_counter, book),
      Term::Lnk { .. } => {}
      Term::Ref { def_id } => {
        if book.is_generated_rule(*def_id) {
          let def = &book.defs[def_id.0 as usize];
          assert!(def.rules.len() == 1);

          let mut term = def.rules[0].body.clone();
          term.fix_names(id_counter, book);

          *self = term
        }
      }
      Term::App { fun, arg } => {
        fun.fix_names(id_counter, book);
        arg.fix_names(id_counter, book);
      }
      Term::If { cond, then, els_ } => {
        cond.fix_names(id_counter, book);
        then.fix_names(id_counter, book);
        els_.fix_names(id_counter, book);
      }
      Term::Dup { fst, snd, val, nxt } => {
        val.fix_names(id_counter, book);

        if let Some(nam) = fst {
          let name = var_id_to_name(*id_counter);
          *id_counter += 1;

          nxt.subst(nam, &Term::Var { nam: name.clone() });
          fst.replace(name);
        }

        if let Some(nam) = snd {
          let name = var_id_to_name(*id_counter);
          *id_counter += 1;

          nxt.subst(nam, &Term::Var { nam: name.clone() });
          snd.replace(name);
        }

        nxt.fix_names(id_counter, book);
      }
      Term::Sup { fst, snd } => {
        fst.fix_names(id_counter, book);
        snd.fix_names(id_counter, book);
      }
      Term::Era => {}
      Term::Num { .. } => {}
      Term::Opx { op: _, fst, snd } => {
        fst.fix_names(id_counter, book);
        snd.fix_names(id_counter, book);
      }
      Term::Let { .. } => unreachable!(),
    }
  }
}
