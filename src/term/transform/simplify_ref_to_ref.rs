use crate::{
  diagnostics::{Diagnostics, ToStringVerbose},
  term::{Ctx, Name, Term},
};
use std::collections::BTreeMap;

#[derive(Debug, Clone)]
pub struct CyclicDefErr;

impl Ctx<'_> {
  /// Substitutes all references of functions that are just
  /// references to other functions by the function they reference.
  ///
  /// In case there is a long chain of ref-to-ref-to-ref, we
  /// substitute values by the last function in the chain.
  ///
  /// ### Example:
  /// ```hvm
  /// A = @x @y (x y)
  /// B = A
  /// C = B
  /// main = @x (C x)
  /// ```
  /// becomes
  /// ```hvm
  /// A = @x @y (x y)
  /// B = A
  /// C = A
  /// main = @x (A x)
  /// ```
  /// Functions `B` and `C` will no longer be referenced anywhere in
  /// the program.
  pub fn simplify_ref_to_ref(&mut self) -> Result<(), Diagnostics> {
    self.info.start_pass();

    let mut ref_map: BTreeMap<Name, Name> = BTreeMap::new();
    // Find to which defs we're mapping the ones that are just references.
    'outer: for def_name in self.book.defs.keys() {
      let mut ref_name = def_name;
      let mut is_ref_to_ref = false;
      while let Term::Ref { nam: next_ref } = &self.book.defs.get(ref_name).unwrap().rule().body {
        if next_ref == ref_name {
          self.info.add_rule_error(CyclicDefErr, def_name.clone());
          continue 'outer;
        }
        ref_name = next_ref;
        is_ref_to_ref = true;
      }
      if is_ref_to_ref {
        ref_map.insert(def_name.clone(), ref_name.clone());
      }
    }

    // Substitute all the occurrences of ref-to-ref.
    for body in self.book.defs.values_mut().map(|def| &mut def.rule_mut().body) {
      subst_ref_to_ref(body, &ref_map);
    }

    self.info.fatal(())
  }
}

/// Returns whether any substitution happened within the term or not
pub fn subst_ref_to_ref(term: &mut Term, ref_map: &BTreeMap<Name, Name>) -> bool {
  Term::recursive_call(move || match term {
    Term::Ref { nam: def_name } => {
      if let Some(target_name) = ref_map.get(def_name) {
        *def_name = target_name.clone();
        true
      } else {
        false
      }
    }

    _ => {
      let mut subst = false;
      for child in term.children_mut() {
        subst |= subst_ref_to_ref(child, ref_map);
      }
      subst
    }
  })
}

impl ToStringVerbose for CyclicDefErr {
  fn to_string_verbose(&self, _verbose: bool) -> String {
    "Definition is a reference to itself.".to_string()
  }
}
