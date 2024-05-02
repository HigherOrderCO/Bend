use hvmc::ast::{Book, Net, Tree};
use std::collections::{HashMap, HashSet};

use crate::maybe_grow;

/// Reorder redexes in the book to have more efficient execution.
///
/// Especially for hvm-cuda, we want to keep the number of nodes/vars/redexes
/// low so we put redexes with recursive refs last (at the bottom of the stack).
pub fn reorder_redexes_recursive_last(book: &mut Book) {
  // Direct dependencies
  let deps = book.iter().map(|(nam, net)| (nam.clone(), dependencies(net))).collect::<HashMap<_, _>>();

  // Look at dependencies to find if recursive
  let recursive_nets = cycles(&deps).into_iter().flatten().collect::<HashSet<_>>();

  for net in book.values_mut() {
    reorder_redexes_net(net, &recursive_nets);
  }
}

/// Reorder redexes to have recursive last (bottom of the stack).
fn reorder_redexes_net(net: &mut Net, recursive_nets: &HashSet<String>) {
  let mut recursive_redexes = vec![];
  let mut non_recursive_redexes = vec![];

  for (a, b) in std::mem::take(&mut net.redexes) {
    if tree_has_recursive(&a, recursive_nets) || tree_has_recursive(&b, recursive_nets) {
      recursive_redexes.push((a, b));
    } else {
      non_recursive_redexes.push((a, b));
    }
  }
  let mut redexes = recursive_redexes;
  redexes.append(&mut non_recursive_redexes);
  net.redexes = redexes;
}

/// Whether a tree has a reference to a recursive net or not.
fn tree_has_recursive(tree: &Tree, recursive_nets: &HashSet<String>) -> bool {
  maybe_grow(|| {
    if let Tree::Ref { nam } = tree {
      recursive_nets.contains(nam)
    } else {
      for child in tree.children() {
        if tree_has_recursive(child, recursive_nets) {
          return true;
        }
      }
      false
    }
  })
}

type DepGraph = HashMap<String, HashSet<String>>;
type Cycles = Vec<Vec<String>>;

/// Find all cycles in the dependency graph.
pub fn cycles(deps: &DepGraph) -> Cycles {
  let mut cycles = vec![];
  let mut stack = vec![];
  let mut visited = HashSet::new();
  for nam in deps.keys() {
    if !visited.contains(nam) {
      find_cycles(deps, nam, &mut visited, &mut stack, &mut cycles);
    }
  }
  cycles
}

fn find_cycles(
  deps: &DepGraph,
  nam: &String,
  visited: &mut HashSet<String>,
  stack: &mut Vec<String>,
  cycles: &mut Cycles,
) {
  maybe_grow(|| {
    // Check if the current ref is already in the stack, which indicates a cycle.
    if let Some(cycle_start) = stack.iter().position(|n| n == nam) {
      // If found, add the cycle to the cycles vector.
      cycles.push(stack[cycle_start ..].to_vec());
      return;
    }
    // If the ref has not been visited yet, mark it as visited.
    if visited.insert(nam.clone()) {
      // Add the current ref to the stack to keep track of the path.
      stack.push(nam.clone());
      // Get the dependencies of the current ref.
      if let Some(dependencies) = deps.get(nam) {
        // Search for cycles from each dependency.
        for dep in dependencies {
          find_cycles(deps, dep, visited, stack, cycles);
        }
      }
      stack.pop();
    }
  })
}

/// Gather the set of net that this net directly depends on (has a ref in the net).
fn dependencies(net: &Net) -> HashSet<String> {
  let mut deps = HashSet::new();
  dependencies_tree(&net.root, &mut deps);
  for (a, b) in &net.redexes {
    dependencies_tree(a, &mut deps);
    dependencies_tree(b, &mut deps);
  }
  deps
}

fn dependencies_tree(tree: &Tree, deps: &mut HashSet<String>) {
  if let Tree::Ref { nam } = tree {
    deps.insert(nam.clone());
  } else {
    for subtree in tree.children() {
      dependencies_tree(subtree, deps);
    }
  }
}
