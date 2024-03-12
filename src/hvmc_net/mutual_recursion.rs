use hvmc::ast::{Book, Tree};
use indexmap::{IndexMap, IndexSet};
use std::fmt::Debug;

type Ref = String;
type Stack<T> = Vec<T>;
type RefSet = IndexSet<Ref>;

#[derive(Default)]
pub struct Graph(IndexMap<Ref, RefSet>);

pub fn check_cycles(book: &Book) -> Result<(), String> {
  let graph = Graph::from(book);
  let cycles = graph.cycles();

  if cycles.is_empty() { Ok(()) } else { Err(pretty_print_cycles(&cycles)) }
}

fn pretty_print_cycles(cycles: &[Vec<Ref>]) -> String {
  cycles
    .iter()
    .enumerate()
    .map(|(i, cycles)| format!("Cycle {}: {}", 1 + i, cycles.join(" -> ")))
    .collect::<Vec<String>>()
    .join("\n")
}

impl Graph {
  pub fn cycles(&self) -> Vec<Vec<Ref>> {
    let mut cycles = Vec::new();
    let mut stack = Stack::new();
    let mut visited = RefSet::new();

    for r#ref in self.0.keys() {
      if !visited.contains(r#ref) {
        self.find_cycles(r#ref, &mut visited, &mut stack, &mut cycles);
      }
    }

    cycles
  }

  fn find_cycles(
    &self,
    r#ref: &Ref,
    visited: &mut RefSet,
    stack: &mut Stack<Ref>,
    cycles: &mut Vec<Vec<Ref>>,
  ) {
    // Check if the current ref is already in the stack, which indicates a cycle.
    if let Some(cycle_start) = stack.iter().position(|n| n == r#ref) {
      // If found, add the cycle to the cycles vector.
      cycles.push(stack[cycle_start ..].to_vec());
      return;
    }

    // If the ref has not been visited yet, mark it as visited.
    if visited.insert(r#ref.clone()) {
      // Add the current ref to the stack to keep track of the path.
      stack.push(r#ref.clone());

      // Get the dependencies of the current ref.
      if let Some(dependencies) = self.get(r#ref) {
        // Search for cycles from each dependency.
        for dep in dependencies {
          self.find_cycles(dep, visited, stack, cycles);
        }
      }

      stack.pop();
    }
  }
}

fn collect_refs(current: Ref, tree: &Tree, graph: &mut Graph) {
  match tree {
    Tree::Ref { nam } => graph.add(current, nam.clone()),
    Tree::Ctr { box lft, rgt, .. } => {
      if let Tree::Ref { nam } = lft {
        graph.add(current.clone(), nam.clone());
      }
      collect_refs(current, rgt, graph);
    }
    Tree::Op { rhs: fst, out: snd, .. } | Tree::Mat { sel: fst, ret: snd } => {
      collect_refs(current.clone(), fst, graph);
      collect_refs(current, snd, graph);
    }
    Tree::Era | Tree::Num { .. } | Tree::Var { .. } => (),
  }
}

impl From<&Book> for Graph {
  fn from(book: &Book) -> Self {
    let mut graph = Self::new();

    for (r#ref, net) in book.iter() {
      // Collect active refs from root.
      collect_refs(r#ref.clone(), &net.root, &mut graph);
      for (left, _) in net.redexes.iter() {
        // If left is an active reference, add to the graph.
        if let Tree::Ref { nam } = left {
          graph.add(r#ref.clone(), nam.clone());
        }
      }
    }

    graph
  }
}

impl Graph {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn add(&mut self, r#ref: Ref, dependency: Ref) {
    self.0.entry(r#ref).or_default().insert(dependency.clone());
    self.0.entry(dependency).or_default();
  }

  pub fn get(&self, r#ref: &Ref) -> Option<&RefSet> {
    self.0.get(r#ref)
  }
}

impl Debug for Graph {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "Graph{:?}", self.0)
  }
}
