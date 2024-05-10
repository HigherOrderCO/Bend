use crate::{
  diagnostics::{Diagnostics, WarningType, ERR_INDENT_SIZE},
  fun::transform::definition_merge::MERGE_SEPARATOR,
  maybe_grow,
};
use hvmc::ast::{Book, Tree};
use indexmap::{IndexMap, IndexSet};
use std::fmt::Debug;

type Ref = String;
type Stack<T> = Vec<T>;
type RefSet = IndexSet<Ref>;

#[derive(Default)]
pub struct Graph(IndexMap<Ref, RefSet>);

pub fn check_cycles(book: &Book, diagnostics: &mut Diagnostics) -> Result<(), Diagnostics> {
  diagnostics.start_pass();

  let graph = Graph::from(book);
  let cycles = graph.cycles();

  if !cycles.is_empty() {
    let msg = format!(include_str!("mutual_recursion.message"), cycles = show_cycles(cycles));
    diagnostics.add_book_warning(msg.as_str(), WarningType::RecursionCycle);
  }

  diagnostics.fatal(())
}
fn show_cycles(mut cycles: Vec<Vec<Ref>>) -> String {
  let tail = if cycles.len() > 5 {
    format!("\n{:ERR_INDENT_SIZE$}and {} other cycles...", "", cycles.len() - 5)
  } else {
    String::new()
  };

  cycles = cycles.into_iter().flat_map(combinations_from_merges).collect::<Vec<_>>();

  let mut cycles = cycles
    .iter()
    .take(5)
    .map(|cycle| {
      let cycle_str = cycle
        .iter()
        .filter(|nam| !nam.contains("__C"))
        .chain(cycle.first())
        .cloned()
        .collect::<Vec<_>>()
        .join(" -> ");
      format!("{:ERR_INDENT_SIZE$}* {}", "", cycle_str)
    })
    .collect::<Vec<String>>()
    .join("\n");

  cycles.push_str(&tail);

  cycles
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

/// Collect active refs from the tree.
fn collect_refs(current: Ref, tree: &Tree, graph: &mut Graph) {
  maybe_grow(|| match tree {
    Tree::Ref { nam, .. } => graph.add(current, nam.clone()),
    Tree::Ctr { ports, .. } => {
      if let Some(last) = ports.last() {
        collect_refs(current, last, graph);
      }
    }
    tree => {
      for subtree in tree.children() {
        collect_refs(current.clone(), subtree, graph);
      }
    }
  });
}

impl From<&Book> for Graph {
  fn from(book: &Book) -> Self {
    let mut graph = Self::new();

    for (r#ref, net) in book.iter() {
      // Collect active refs from the root.
      collect_refs(r#ref.clone(), &net.root, &mut graph);

      // Collect active refs from redexes.
      for (_, left, right) in net.redexes.iter() {
        if let Tree::Ref { nam, .. } = left {
          graph.add(r#ref.clone(), nam.clone());
        }
        if let Tree::Ref { nam, .. } = right {
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

fn combinations_from_merges(cycle: Vec<Ref>) -> Vec<Vec<Ref>> {
  let mut combinations: Vec<Vec<Ref>> = vec![vec![]];
  for r#ref in cycle {
    if let Some(index) = r#ref.find(MERGE_SEPARATOR) {
      let (left, right) = r#ref.split_at(index);
      let right = &right[MERGE_SEPARATOR.len() ..]; // skip merge separator
      let mut new_combinations = Vec::new();
      for combination in &combinations {
        let mut left_comb = combination.clone();
        left_comb.push(left.to_string());
        new_combinations.push(left_comb);

        let mut right_comb = combination.clone();
        right_comb.push(right.to_string());
        new_combinations.push(right_comb);
      }
      combinations = new_combinations;
    } else {
      for combination in &mut combinations {
        combination.push(r#ref.clone());
      }
    }
  }
  combinations
}
