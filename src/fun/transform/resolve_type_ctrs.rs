use crate::{
  diagnostics::Diagnostics,
  fun::{Adts, Ctx, Type},
  maybe_grow,
};

impl Ctx<'_> {
  /// Resolves type constructors in the book.
  pub fn resolve_type_ctrs(&mut self) -> Result<(), Diagnostics> {
    for def in self.book.defs.values_mut() {
      let res = def.typ.resolve_type_ctrs(&self.book.adts);
      self.info.take_rule_err(res, def.name.clone());
    }

    let adts = self.book.adts.clone();
    for adt in self.book.adts.values_mut() {
      for ctr in adt.ctrs.values_mut() {
        let res = ctr.typ.resolve_type_ctrs(&adts);
        self.info.take_rule_err(res, ctr.name.clone());
      }
    }

    self.info.fatal(())
  }
}

impl Type {
  /// Resolves type constructors in the type.
  pub fn resolve_type_ctrs(&mut self, adts: &Adts) -> Result<(), String> {
    maybe_grow(|| {
      match self {
        Type::Var(nam) => {
          // If the variable actually refers to a type, we change the type to a constructor.
          if adts.contains_key(nam) {
            eprintln!("found adt: {nam}");
            *self = Type::Ctr(nam.clone(), vec![]);
          }
        }
        Type::Ctr(name, args) => {
          if !adts.contains_key(name) {
            return Err(format!("Found unknown type constructor '{name}'."));
          }
          for arg in args {
            arg.resolve_type_ctrs(adts)?;
          }
        }
        Type::Tup(els) => {
          for el in els {
            el.resolve_type_ctrs(adts)?;
          }
        }
        Type::Arr(lft, rgt) => {
          lft.resolve_type_ctrs(adts)?;
          rgt.resolve_type_ctrs(adts)?;
        }
        Type::Number(t) | Type::Integer(t) => t.resolve_type_ctrs(adts)?,
        Type::Any | Type::Hole | Type::None | Type::U24 | Type::I24 | Type::F24 => {}
      }
      Ok(())
    })
  }
}
