use super::{id_to_name, DefId};
use hvm_core::{show_lnet, LNet};
use itertools::Itertools;
use std::{collections::HashMap, fmt};

pub struct Book {
  pub defs: HashMap<DefId, LNet>,
}

impl fmt::Display for Book {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for (id, net) in self.defs.iter().sorted_unstable_by_key(|(id, _)| *id) {
      writeln!(f, "{} =", id_to_name(**id))?;
      writeln!(f, "{}", show_lnet(net).split('\n').map(|x| format!("  {x}")).join("\n"))?;
    }
    Ok(())
  }
}
