use super::tree_children;
use crate::hvm::ast::{Book, Net, Tree};
use crate::maybe_grow;
use std::collections::{HashMap, HashSet};

pub fn add_recursive_priority(book: &mut Book) {
  // Direct dependencies
  let deps = book.defs.iter().map(|(nam, net)| (nam.clone(), dependencies(net))).collect::<HashMap<_, _>>();
  // Recursive cycles
  let cycles = cycles(&deps);

  for cycle in cycles {
    // For each function in the cycle, if there are redexes with the
    // next ref in the cycle, add a priority to one of those redexes.
    for i in 0..cycle.len() {
      let cur = book.defs.get_mut(&cycle[i]).unwrap();
      let nxt = &cycle[(i + 1) % cycle.len()];
      add_priority_next_in_cycle(cur, nxt);
    }
  }
}

fn add_priority_next_in_cycle(net: &mut Net, nxt: &String) {
  let mut count = 0;

  // Count the number of recursive refs
  for (_, a, b) in net.rbag.iter() {
    if let Tree::Ref { nam } = a {
      if nam == nxt {
        count += 1;
      }
    }
    if let Tree::Ref { nam } = b {
      if nam == nxt {
        count += 1;
      }
    }
  }

  // If there are more than one recursive ref, add a priority to them.
  if count > 1 {
    for (pri, a, b) in net.rbag.iter_mut().rev() {
      if let Tree::Ref { nam } = a {
        if nam == nxt {
          *pri = true;
        }
      }
      if let Tree::Ref { nam } = b {
        if nam == nxt {
          *pri = true;
        }
      }
    }
  }
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
      cycles.push(stack[cycle_start..].to_vec());
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
  for (_, a, b) in &net.rbag {
    dependencies_tree(a, &mut deps);
    dependencies_tree(b, &mut deps);
  }
  deps
}

fn dependencies_tree(tree: &Tree, deps: &mut HashSet<String>) {
  if let Tree::Ref { nam, .. } = tree {
    deps.insert(nam.clone());
  } else {
    for subtree in tree_children(tree) {
      dependencies_tree(subtree, deps);
    }
  }
}
