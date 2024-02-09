use crate::term::{Book, Name, Term};

impl Book {
  /// When main is simply directly calling another function, we substitute the ref with a copy of its body.
  ///
  /// This is performed because Hvm-Core produces inconsistent outputs in the parallel mode when such pattern is present
  pub fn simplify_main_ref(&mut self, main: &Name) {
    while let Term::Ref { nam: def_name } = &self.defs[main].rule().body {
      let rule_body = self.defs[def_name].rule().body.clone();
      let main_body = &mut self.defs.get_mut(main).unwrap().rule_mut().body;
      *main_body = rule_body;
    }
  }
}
