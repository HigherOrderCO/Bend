use crate::term::{Book, DefId, Term};

impl Book {
  /// When main is simply directly calling another function, we substitute the ref with a copy of its body.
  /// 
  /// This is performed because Hvm-Core produces inconsistent outputs in the parallel mode when such pattern is present
  pub fn simplify_main_ref(&mut self, main: DefId) {
    if let Term::Ref { def_id } = &self.defs.get(&main).unwrap().rules[0].body {
      let rule_body = self.defs.get(def_id).unwrap().rules[0].body.clone();
      let main_body = &mut self.defs.get_mut(&main).unwrap().rules[0].body;
      *main_body = rule_body;
    }
  }
}
