use crate::ENTRY_POINT;
use hvmc::{
  ast::{val_to_name, Book, Tree},
  run::Val,
};
use std::collections::HashSet;

pub fn prune_defs(book: &mut Book) {
  let mut used_defs = HashSet::new();
  // On hvmc, the entry point is always "main"
  let mut to_visit = vec![ENTRY_POINT.to_string()];

  while let Some(nam) = to_visit.pop() {
    let def = &book[&nam];
    used_defs_in_tree(&def.root, &mut used_defs, &mut to_visit);
    for (a, b) in &def.rdex {
      used_defs_in_tree(a, &mut used_defs, &mut to_visit);
      used_defs_in_tree(b, &mut used_defs, &mut to_visit);
    }
  }
  let used_defs = used_defs.into_iter().map(val_to_name).collect::<HashSet<_>>();
  book.retain(|nam, _| used_defs.contains(nam) || nam == ENTRY_POINT);
}

fn used_defs_in_tree(tree: &Tree, used_defs: &mut HashSet<Val>, to_visit: &mut Vec<String>) {
  match tree {
    Tree::Ref { nam } => {
      if used_defs.insert(*nam) {
        to_visit.push(val_to_name(*nam));
      }
    }
    Tree::Con { lft, rgt }
    | Tree::Tup { lft, rgt }
    | Tree::Dup { lft, rgt, .. }
    | Tree::Op2 { lft, rgt, .. }
    | Tree::Mat { sel: lft, ret: rgt } => {
      used_defs_in_tree(lft, used_defs, to_visit);
      used_defs_in_tree(rgt, used_defs, to_visit);
    }
    Tree::Op1 { rgt, .. } => {
      used_defs_in_tree(rgt, used_defs, to_visit);
    }
    Tree::Var { .. } | Tree::Num { .. } | Tree::Era => (),
  }
}
