use crate::ast::{
  compat::{
    addr, enter, kind, link, new_inet, new_node, port, slot, INet, INode, INodes, NodeId, NodeKind, Port,
    SlotId, CON, DUP, ERA, LABEL_MASK, NUM, NUMOP, REF, ROOT, TAG_MASK,
  },
  Name, Number, Term,
};
use hvm_core::{u64_to_name, LNet, LTree};
use std::collections::{HashMap, HashSet};

pub fn readback_net(net: &LNet) -> anyhow::Result<Term> {
  let core_net = core_net_to_compat(net)?;
  let term = readback_compat(&core_net);
  Ok(term)
}

fn core_net_to_compat(net: &LNet) -> anyhow::Result<INet> {
  let mut inodes = vec![];
  let mut n_vars = 0;
  let net_root = if let LTree::Var { nam } = &net.root { nam } else { "" };

  if !matches!(&net.root, LTree::Var { .. }) {
    let mut root = tree_to_inodes(&net.root, "_".to_string(), net_root, &mut n_vars);
    inodes.append(&mut root);
  }

  for (i, (tree1, tree2)) in net.acts.iter().enumerate() {
    let tree_root = format!("a{i}");
    let mut tree1 = tree_to_inodes(tree1, tree_root.clone(), net_root, &mut n_vars);
    inodes.append(&mut tree1);
    let mut tree2 = tree_to_inodes(tree2, tree_root, net_root, &mut n_vars);
    inodes.append(&mut tree2);
  }

  let compat_net = inodes_to_inet(&inodes);
  Ok(compat_net)
}

fn new_var(n_vars: &mut NodeId) -> String {
  let new_var = format!("x{n_vars}");
  *n_vars += 1;
  new_var
}

fn tree_to_inodes(tree: &LTree, tree_root: String, net_root: &str, n_vars: &mut NodeId) -> INodes {
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
      LTree::Nod { tag, lft, rgt } => {
        let kind = if *tag == hvm_core::CON { CON } else { DUP | (*tag - hvm_core::DUP) as NodeKind };
        let var_lft = process_node_subtree(lft, net_root, &mut subtrees, n_vars);
        let var_rgt = process_node_subtree(rgt, net_root, &mut subtrees, n_vars);
        inodes.push(INode { kind, ports: [subtree_root, var_lft, var_rgt] })
      }
      LTree::Var { .. } => unreachable!(),
      LTree::Ref { nam } => {
        let kind = REF | (*nam as NodeKind);
        let var = new_var(n_vars);
        inodes.push(INode { kind, ports: [subtree_root, var.clone(), var] });
      }
      LTree::NUM { val } => {
        let kind = NUM | (*val as NodeKind);
        let var = new_var(n_vars);
        inodes.push(INode { kind, ports: [subtree_root, var.clone(), var] });
      }
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
fn readback_compat(net: &INet) -> Term {
  // Given a port, returns its name, or assigns one if it wasn't named yet.
  fn name_of(net: &INet, var_port: Port, var_name: &mut HashMap<Port, Name>) -> Name {
    // If port is linked to an erase node, return an unused variable
    if kind(net, addr(enter(net, var_port))) == ERA {
      return Name("*".to_string());
    }
    if !var_name.contains_key(&var_port) {
      let mut n = var_name.len();
      let mut name = String::new();
      loop {
        let c = (n % 26) as u8 + b'a';
        name.push(c as char);
        n /= 26;
        if n == 0 {
          break;
        }
      }
      var_name.insert(var_port, Name(name));
    }
    var_name.get(&var_port).unwrap().clone()
  }

  // Reads a term recursively by starting at root node.
  fn reader(
    net: &INet,
    next: Port,
    var_name: &mut HashMap<Port, Name>,
    dups_vec: &mut Vec<NodeId>,
    dups_set: &mut HashSet<NodeId>,
    seen: &mut HashSet<Port>,
  ) -> Term {
    if seen.contains(&next) {
      return Term::Var { nam: Name("...".to_string()) };
    }

    seen.insert(next);

    let kind_ = kind(net, addr(next));
    let tag = kind_ & TAG_MASK;
    let label = kind_ & LABEL_MASK;

    match tag {
      // If we're visiting a set...
      ERA => Term::Era,
      // If we're visiting a con node...
      CON => match slot(next) {
        // If we're visiting a port 0, then it is a lambda.
        0 => {
          let nam = name_of(net, port(addr(next), 1), var_name);
          let nam = if *nam == "*" { None } else { Some(nam) };
          let prt = enter(net, port(addr(next), 2));
          let bod = reader(net, prt, var_name, dups_vec, dups_set, seen);
          Term::Lam { nam, bod: Box::new(bod) }
        }
        // If we're visiting a port 1, then it is a variable.
        1 => {
          Term::Var { nam: name_of(net, next, var_name) }
          //Var{nam: format!("{}@{}", String::from_utf8_lossy(&name_of(net, next, var_name)), addr(next)).into()}
        }
        // If we're visiting a port 2, then it is an application.
        _ => {
          let prt = enter(net, port(addr(next), 0));
          let fun = reader(net, prt, var_name, dups_vec, dups_set, seen);
          let prt = enter(net, port(addr(next), 1));
          let arg = reader(net, prt, var_name, dups_vec, dups_set, seen);
          Term::App { fun: Box::new(fun), arg: Box::new(arg) }
        }
      },
      REF => Term::Var { nam: Name(u64_to_name(label)) },
      // If we're visiting a fan node...
      DUP => match slot(next) {
        // If we're visiting a port 0, then it is a pair.
        0 => {
          let prt = enter(net, port(addr(next), 1));
          let fst = reader(net, prt, var_name, dups_vec, dups_set, seen);
          let prt = enter(net, port(addr(next), 2));
          let snd = reader(net, prt, var_name, dups_vec, dups_set, seen);
          Term::Sup { fst: Box::new(fst), snd: Box::new(snd) }
        }
        // If we're visiting a port 1 or 2, then it is a variable.
        // Also, that means we found a dup, so we store it to read later.
        _ => {
          if !dups_set.contains(&addr(next)) {
            dups_set.insert(addr(next));
            dups_vec.push(addr(next));
          }
          //Var{nam: format!("{}@{}", String::from_utf8_lossy(&name_of(net, next, var_name)), addr(next)).into()}
          Term::Var { nam: name_of(net, next, var_name) }
        }
      },
      NUM => Term::Num { val: Number(label) },
      NUMOP => todo!(),
      _ => unreachable!(),
    }
  }

  // A hashmap linking ports to binder names. Those ports have names:
  // Port 1 of a con node (λ), ports 1 and 2 of a fan node (let).
  let mut binder_name = HashMap::new();

  // Dup aren't scoped. We find them when we read one of the variables
  // introduced by them. Thus, we must store the dups we find to read later.
  // We have a vec for .pop(). and a set to avoid storing duplicates.
  let mut dups_vec = Vec::new();
  let mut dups_set = HashSet::new();
  let mut seen = HashSet::new();

  // Reads the main term from the net
  let mut main = reader(net, enter(net, ROOT), &mut binder_name, &mut dups_vec, &mut dups_set, &mut seen);

  // Reads let founds by starting the reader function from their 0 ports.
  while let Some(dup) = dups_vec.pop() {
    let val =
      reader(net, enter(net, port(dup, 0)), &mut binder_name, &mut dups_vec, &mut dups_set, &mut seen);
    let fst = name_of(net, port(dup, 1), &mut binder_name);
    let snd = name_of(net, port(dup, 2), &mut binder_name);
    let fst = if *fst == "*" { None } else { Some(fst) };
    let snd = if *snd == "*" { None } else { Some(snd) };
    let val = Box::new(val);
    let nxt = Box::new(main);
    main = Term::Dup { fst, snd, val, nxt };
  }
  main
}
