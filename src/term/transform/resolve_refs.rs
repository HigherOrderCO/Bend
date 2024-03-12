use crate::{
  diagnostics::{Diagnostics, ToStringVerbose},
  builtins::CORE_BUILTINS,
  term::{Ctx, Name, Pattern, Term},
};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub struct ReferencedMainErr;

impl Ctx<'_> {
  /// Decides if names inside a term belong to a Var or to a Ref.
  /// Converts `Term::Var(nam)` into `Term::Ref(nam)` when the name
  /// refers to a function definition and there is no variable in
  /// scope shadowing that definition.
  ///
  /// Precondition: Refs are encoded as vars, Constructors are resolved.
  ///
  /// Postcondition: Refs are encoded as refs, with the correct def id.
  pub fn resolve_refs(&mut self) -> Result<(), Diagnostics> {
    self.info.start_pass();

    let def_names = self.book.defs.keys().cloned().collect::<HashSet<_>>();
    for (def_name, def) in &mut self.book.defs {
      for rule in def.rules.iter_mut() {
        let mut scope = HashMap::new();

        for name in rule.pats.iter().flat_map(Pattern::binds) {
          push_scope(name.as_ref(), &mut scope);
        }

        let res = rule.body.resolve_refs(&def_names, self.book.entrypoint.as_ref(), &mut scope);
        self.info.take_rule_err(res, def_name.clone());
      }
    }

    self.info.fatal(())
  }
}

impl Term {
  pub fn resolve_refs<'a>(
    &'a mut self,
    def_names: &HashSet<Name>,
    main: Option<&Name>,
    scope: &mut HashMap<&'a Name, usize>,
  ) -> Result<(), ReferencedMainErr> {
    Term::recursive_call(move || {
      if let Term::Var { nam } = self
        && is_var_in_scope(nam, scope)
      {
        // If the variable is actually a reference to main, don't swap and return an error.
        if let Some(main) = main
          && nam == main
        {
          return Err(ReferencedMainErr);
        }

        // If the variable is actually a reference to a function, swap the term.
        if def_names.contains(nam) || CORE_BUILTINS.contains(&nam.0.as_ref()) {
          *self = Term::r#ref(nam);
        }
      }

      for (child, binds) in self.children_mut_with_binds() {
        for bind in binds.clone() {
          push_scope(bind.as_ref(), scope);
        }
        child.resolve_refs(def_names, main, scope)?;
        for bind in binds.rev() {
          pop_scope(bind.as_ref(), scope);
        }
      }
      Ok(())
    })
  }
}

fn push_scope<'a>(name: Option<&'a Name>, scope: &mut HashMap<&'a Name, usize>) {
  if let Some(name) = name {
    let var_scope = scope.entry(name).or_default();
    *var_scope += 1;
  }
}

fn pop_scope<'a>(name: Option<&'a Name>, scope: &mut HashMap<&'a Name, usize>) {
  if let Some(name) = name {
    let var_scope = scope.entry(name).or_default();
    *var_scope -= 1;
  }
}

fn is_var_in_scope<'a>(name: &'a Name, scope: &HashMap<&'a Name, usize>) -> bool {
  match scope.get(name) {
    Some(entry) => *entry == 0,
    None => true,
  }
}

impl ToStringVerbose for ReferencedMainErr {
  fn to_string_verbose(&self, _verbose: bool) -> String {
    "Main definition can't be referenced inside the program.".to_string()
  }
}
