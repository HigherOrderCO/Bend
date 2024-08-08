use indexmap::IndexSet;

use crate::{
  diagnostics::Diagnostics,
  fun::{Adts, Ctx, Name, Type},
  maybe_grow,
};

impl Ctx<'_> {
  /// Resolves type constructors in the book and adds for-alls for the free type vars.
  pub fn resolve_type_ctrs(&mut self) -> Result<(), Diagnostics> {
    for def in self.book.defs.values_mut() {
      let mut free_vars = Default::default();
      match def.typ.resolve_type_ctrs(&mut vec![], &mut free_vars, &self.book.adts) {
        Ok(_) => {
          let typ = std::mem::replace(&mut def.typ, Type::Hole);
          def.typ = free_vars.into_iter().rfold(typ, |acc, nam| Type::All(nam, Box::new(acc)));
        }
        Err(e) => self.info.add_function_error(e, def.name.clone(), def.source.clone()),
      }
    }

    self.info.fatal(())
  }
}

impl Type {
  /// Resolves type constructors in the type.
  pub fn resolve_type_ctrs(
    &mut self,
    scope: &mut Vec<Name>,
    free_vars: &mut IndexSet<Name>,
    adts: &Adts,
  ) -> Result<(), String> {
    maybe_grow(|| {
      match self {
        Type::Var(nam) => {
          // If the variable actually refers to a type, we change the type to a constructor.
          if adts.get(nam).is_some() {
            // Only change if the name is not being shadowed by a forall.
            if !scope.contains(nam) {
              *self = Type::Ctr(nam.clone(), vec![]);
            }
          } else if !scope.contains(nam) && !free_vars.contains(nam) {
            // A new free variable, add it to the free var set to be added as a for all later.
            free_vars.insert(nam.clone());
          }
        }
        Type::Ctr(name, args) => {
          if !adts.contains_key(name) {
            return Err(format!("Found unknown type constructor '{name}'."));
          }
          for arg in args {
            arg.resolve_type_ctrs(scope, free_vars, adts)?;
          }
        }
        Type::All(nam, body) => {
          scope.push(nam.clone());
          body.resolve_type_ctrs(scope, free_vars, adts)?;
          scope.pop();
        }
        Type::Arr(lft, rgt) => {
          lft.resolve_type_ctrs(scope, free_vars, adts)?;
          rgt.resolve_type_ctrs(scope, free_vars, adts)?;
        }
        Type::Tup(els) => {
          for el in els {
            el.resolve_type_ctrs(scope, free_vars, adts)?;
          }
        }
        Type::Any | Type::Hole | Type::None | Type::U24 | Type::I24 | Type::F24 => {}
      }
      Ok(())
    })
  }
}
