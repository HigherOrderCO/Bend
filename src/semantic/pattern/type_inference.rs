use super::{Adt, AdtId, Type};
use crate::{
  ast::{DefId, Definition, DefinitionBook, Name},
  term::Pattern,
};
use anyhow::anyhow;
use itertools::Itertools;
use std::collections::HashMap;

type Adts = HashMap<AdtId, Adt>;
type Types = Vec<Vec<Type>>;

impl DefinitionBook {
  /// Infers ADTs from the patterns of the rules in a book.
  /// Returns the infered type of the patterns of each definition.
  /// Returns an error if rules use the types in a inconsistent way.
  /// These could be same name in different types, different arities or mixing numbers and ADTs.
  /// Precondition: Rules have been flattened, rule arity is correct.
  pub fn get_types_from_patterns(&self) -> anyhow::Result<(Adts, Types)> {
    // TODO: This algorithm does a lot of unnecessary copying, checking and moving around of data.
    // There's a lot of space for optimizing.

    // The infered ADTs, possibly shared between multiple defs
    let mut adts: HashMap<AdtId, Adt> = HashMap::new();
    // The types of each pattern in each def.
    let mut types: Vec<Vec<Type>> = vec![];
    // Control
    let mut pats_using_adt: HashMap<AdtId, Vec<(DefId, usize)>> = HashMap::new();
    let mut ctr_name_to_adt: HashMap<Name, AdtId> = HashMap::new();
    let mut adt_counter = 0;

    for def in &self.defs {
      let pat_types = get_types_from_def_patterns(def)?;
      // Check if the types in this def share some ctr names with previous types.
      // Try to merge them if they do.
      for (i, typ) in pat_types.iter().enumerate() {
        if let Type::Adt(_) = typ {
          let mut crnt_adt = def.get_adt_from_pat(i)?;
          eprintln!("crnt_adt: {crnt_adt}");
          // Gather the existing types that share constructor names.
          // We will try to merge them all together.
          let mut to_merge = vec![];
          for ctr_name in crnt_adt.ctrs.keys() {
            if let Some(old_adt_idx) = ctr_name_to_adt.get(ctr_name) {
              to_merge.push(*old_adt_idx);
            }
          }
          if to_merge.is_empty() {
            // If nothing to merge, just add the new type and update the control vars
            for ctr in crnt_adt.ctrs.keys() {
              pats_using_adt.insert(adt_counter, vec![(def.def_id, i)]);
              ctr_name_to_adt.insert(ctr.clone(), adt_counter);
            }
            adts.insert(adt_counter, crnt_adt);
            adt_counter += 1;
          } else {
            // If this adt has to be merged with others, we use the id of the first existing adt to store it.
            // We merge all the adts sharing constructor names with our current adt into the one with the first id.
            // All the other adts are removed in favor of the merged one that has all the constructors.
            // The control variables are updated to now point everything to the merged adt.
            let dest_id = to_merge[0];
            pats_using_adt.get_mut(&dest_id).unwrap().push((def.def_id, i));
            for to_merge in to_merge {
              merge_adts(
                &mut crnt_adt,
                dest_id,
                &mut adts,
                to_merge,
                &mut pats_using_adt,
                &mut ctr_name_to_adt,
              )?;
            }
            adts.insert(dest_id, crnt_adt);
          }
        }
      }
      types.push(pat_types);
    }
    // Point the definition types to the correct adts.
    for (adt_id, uses) in pats_using_adt {
      for (def_id, pat_id) in uses {
        if let Type::Adt(type_adt) = &mut types[*def_id as usize][pat_id] {
          *type_adt = adt_id;
        }
      }
    }
    Ok((adts, types))
  }
}

fn get_types_from_def_patterns(def: &Definition) -> anyhow::Result<Vec<Type>> {
  let arity = def.arity();
  let mut pat_types = vec![];
  for pat_idx in 0 .. arity {
    let pats = def.rules.iter().map(|x| &x.pats[pat_idx]);
    let mut pat_type = Type::Any;
    for pat in pats {
      if let Pattern::Ctr(..) = pat {
        pat_type = Type::Adt(usize::MAX);
      }
    }
    pat_types.push(pat_type);
  }

  Ok(pat_types)
}

impl Definition {
  fn get_adt_from_pat(&self, pat_idx: usize) -> anyhow::Result<Adt> {
    let mut adt = Adt::new();
    for rule in &self.rules {
      match &rule.pats[pat_idx] {
        Pattern::Ctr(nam, args) => {
          if let Some(expected_arity) = adt.ctrs.get(nam) {
            if *expected_arity == self.arity() {
            } else {
              return Err(anyhow::anyhow!(
                "Inconsistent arity used for constructor {nam} across different rules for the same definition argument"
              ));
            }
          } else {
            adt.ctrs.insert(nam.clone(), args.len());
          }
        }
        Pattern::Var(_) => adt.others = true,

        _ => panic!("Expected only Ctr and Var patterns to be called here"),
      }
    }
    Ok(adt)
  }
}

fn merge_adts(
  this_adt: &mut Adt,
  this_id: AdtId,
  adts: &mut HashMap<AdtId, Adt>,
  other_id: AdtId,
  pats_using_adt: &mut HashMap<AdtId, Vec<(DefId, usize)>>,
  ctr_name_to_adt: &mut HashMap<Name, AdtId>,
) -> anyhow::Result<()> {
  let adts2 = adts.clone();
  let other_adt = adts.get_mut(&other_id).unwrap();

  // If all constructors of the other ADT are known, check that this ADT doesn't have any extra constructors.
  // Also check arity of shared constructors.
  for (ctr_name, this_arity) in &this_adt.ctrs {
    if let Some(other_arity) = other_adt.ctrs.get(ctr_name) {
      if this_arity != other_arity {
        return Err(anyhow!(
          "Inconsistent arity used for constructor {ctr_name} across different definition arguments"
        ));
      }
    } else if !other_adt.others {
      eprintln!("Adts: {{{}}}", adts2.iter().map(|(a, b)| format!("{a} => {b}")).join(", "));
      return Err(anyhow!(
        "Found same constructor {ctr_name} being used in incompatible types {this_adt} and {other_adt}"
      ));
    }
  }
  // If not all constructors of this ADT are known, move any new constructors from the other ADT to this one.
  if this_adt.others {
    for (ctr_name, other_arity) in &other_adt.ctrs {
      if !other_adt.ctrs.contains_key(ctr_name) {
        this_adt.ctrs.insert(ctr_name.clone(), *other_arity);
      }
    }
  }
  this_adt.others = this_adt.others && other_adt.others;
  if this_id != other_id {
    for ctr_name in other_adt.ctrs.keys() {
      *ctr_name_to_adt.get_mut(ctr_name).unwrap() = this_id;
    }
    let mut moved_pats = pats_using_adt.remove(&other_id).unwrap();
    pats_using_adt.get_mut(&this_id).unwrap().append(&mut moved_pats);
    adts.remove(&other_id);
  }

  Ok(())
}
