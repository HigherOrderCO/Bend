use super::DefId;
use hvm2::lang::u64_to_name;
use itertools::Itertools;
use std::{collections::HashMap, fmt};

pub use hvm2::{
  core::{Tag, CON, DUP, ERA, REF},
  lang::{show_lnet, show_ltree, show_net, LNet, LTree, OP},
};

pub struct Book {
  pub defs: HashMap<DefId, LNet>,
}

impl fmt::Display for Book {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for (id, net) in self.defs.iter().sorted_unstable_by_key(|(id, _)| *id) {
      writeln!(f, "{} =", u64_to_name(**id))?;
      // TODO: hvm annoyingly prints out a new line after an lnet
      writeln!(f, "{}", show_lnet(net).split('\n').map(|x| format!("  {x}")).join("\n"))?;
    }
    Ok(())
  }
}
