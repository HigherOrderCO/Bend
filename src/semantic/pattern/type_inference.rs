use crate::ast::{hvm_lang::Pattern, DefId, Definition, DefinitionBook, Name};
use anyhow::anyhow;
use itertools::Itertools;
use std::{collections::HashMap, fmt};

impl DefinitionBook {
  /// Infers ADTs from the patterns of the rules in a book.
  /// Returns the infered type of the patterns of each definition.
  /// Returns an error if rules use the types in a inconsistent way.
  /// These could be same name in different types, different arities or mixing numbers and ADTs.
  /// Precondition: Rules have been flattened, rule arity is correct.
  pub fn get_types_from_patterns(&self) -> anyhow::Result<(HashMap<AdtId, Adt>, Vec<Vec<Type>>)> {
    let mut adts: HashMap<AdtId, Adt> = HashMap::new();
    let mut pats_using_adt: HashMap<AdtId, Vec<(DefId, usize)>> = HashMap::new();
    let mut ctr_name_to_adt: HashMap<Name, AdtId> = HashMap::new();
    let mut types: Vec<Vec<Type>> = vec![]; // The type of each 
    let mut adt_counter = 0;
    for def in &self.defs {
      let pat_types = get_types_from_def_patterns(def)?;
      // Check if the types in this def share some ctr names with previous types.
      // Try to merge them if they do
      for (i, typ) in pat_types.iter().enumerate() {
        if let Type::Adt(_) = typ {
          let mut crnt_adt = def.get_adt_from_pat(i)?;
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

#[derive(Debug, Clone)]
pub enum Type {
  Any,
  Adt(AdtId),
  #[cfg(feature = "nums")]
  U32,
  #[cfg(feature = "nums")]
  I32,
}

type AdtId = usize;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Adt {
  // Constructor names and their arities
  ctrs: HashMap<Name, usize>,
  others: bool,
}

impl Adt {
  pub fn from_ctr(nam: Name, arity: usize) -> Self {
    Adt { ctrs: HashMap::from_iter([(nam, arity)]), others: false }
  }

  pub fn new() -> Self {
    Default::default()
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
              return Err(anyhow::anyhow!("Inconsistent arity used for constructor {nam}"));
            }
          } else {
            adt.ctrs.insert(nam.clone(), args.len());
          }
        }
        Pattern::Var(_) => adt.others = true,
        #[cfg(feature = "nums")]
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
  let other_adt = adts.get_mut(&other_id).unwrap();
  if this_adt != other_adt {
    let accept_different = this_adt.others || other_adt.others;
    if accept_different {
      this_adt.others = accept_different;
      for (ctr_name, ctr_arity) in &other_adt.ctrs {
        *ctr_name_to_adt.get_mut(ctr_name).unwrap() = this_id;
        if let Some(old_arity) = this_adt.ctrs.get(ctr_name) {
          if old_arity != ctr_arity {
            return Err(anyhow!("Inconsistent arity used for constructor {ctr_name}"));
          }
        } else {
          this_adt.ctrs.insert(ctr_name.clone(), *ctr_arity);
        }
      }
    } else {
      return Err(anyhow!("Found same constructor being used in incompatible types"));
    }
  }
  if this_id != other_id {
    let mut moved_pats = pats_using_adt.remove(&other_id).unwrap();
    pats_using_adt.get_mut(&this_id).unwrap().append(&mut moved_pats);
    adts.remove(&other_id);
  }

  Ok(())
}

impl fmt::Display for Adt {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "[{}{}]",
      self
        .ctrs
        .iter()
        .map(|(nam, arity)| format!("({}{})", nam, (0 .. *arity).map(|_| format!(" _")).join("")))
        .join(", "),
      if self.others { ", ..." } else { "" }
    )
  }
}
