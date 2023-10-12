use super::DefId;
use crate::term::DefNames;
use hvmc::{show_lnet, val_to_name, LNet};
use itertools::Itertools;
use std::collections::HashMap;

pub struct Book {
  pub defs: HashMap<DefId, LNet>,
  pub main: DefId,
}

impl Book {
  pub fn to_string(&self, def_names: &DefNames) -> String {
    self
      .defs
      .iter()
      .sorted_unstable_by_key(|(id, _)| *id)
      .map(|(id, net)| {
        format!(
          "@{} ({}) =\n{}",
          val_to_name(id.to_internal()),
          def_names.name(id).unwrap(),
          show_lnet(net).split('\n').map(|x| format!("  {x}")).join("\n")
        )
      })
      .join("\n")
  }
}
