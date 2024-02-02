use std::collections::HashMap;

use crate::term::{Book, Name, Pattern};

impl Book {
  /// Checks if every constructor pattern of every definition rule has the same arity from the
  /// defined adt constructor.
  ///
  /// Constructors should be already resolved.
  pub fn check_ctrs_arities(&self) -> Result<(), String> {
    let arities = self.ctr_arities();

    for def in self.defs.values() {
      let def_name = &self.def_names.id_to_name[&def.def_id];
      for rule in def.rules.iter() {
        for pat in rule.pats.iter() {
          pat.check(&arities).map_err(|err| format!("In definition {}: {}", def_name, err))?;
        }
      }
    }

    Ok(())
  }

  /// Returns a hashmap of the constructor name to its arity.
  pub fn ctr_arities(&self) -> HashMap<Name, usize> {
    let mut arities = HashMap::new();

    for adt in self.adts.values() {
      for (ctr_name, fields) in adt.ctrs.iter() {
        arities.insert(ctr_name.clone(), fields.len());
      }
    }

    arities
  }
}

impl Pattern {
  fn check(&self, arities: &HashMap<Name, usize>) -> Result<(), String> {
    match self {
      Pattern::Ctr(name, args) => {
        let arity = arities.get(name).unwrap();
        let args = args.len();
        if *arity != args {
          return Err(format!(
            "Arity error. Constructor '{}' expects {} fields, found {}.",
            name, arity, args
          ));
        } else {
          Ok(())
        }
      }
      Pattern::Tup(fst, snd) => {
        fst.check(arities)?;
        snd.check(arities)
      }
      Pattern::List(els) => {
        for el in els {
          el.check(arities)?;
        }
        Ok(())
      }
      Pattern::Var(..) | Pattern::Num(..) => Ok(()),
    }
  }
}
