use crate::fun::{Book, Definition, Name, Pattern, Rule, Tag, Term};

impl Book {
  /// Defines a function for each constructor in each ADT in the book.
  pub fn encode_adts(&mut self) {
    let mut defs = vec![];
    for adt in self.adts.values() {
      for (ctr_name, fields) in &adt.ctrs {
        let ctrs: Vec<_> = adt.ctrs.keys().cloned().collect();

        let body = encode_ctr(fields.iter().map(|f| &f.nam), ctrs, ctr_name);

        let rules = vec![Rule { pats: vec![], body }];
        let def = Definition { name: ctr_name.clone(), rules, builtin: adt.builtin };
        defs.push((ctr_name.clone(), def));
      }
    }
    self.defs.extend(defs);
  }
}

fn encode_ctr<'a>(
  ctr_args: impl DoubleEndedIterator<Item = &'a Name> + Clone,
  ctrs: Vec<Name>,
  ctr_name: &Name,
) -> Term {
  let tag = Tag::Static;
  let ctr = Term::Var { nam: ctr_name.clone() };
  let app =
    ctr_args.clone().cloned().fold(ctr, |acc, nam| Term::tagged_app(tag.clone(), acc, Term::Var { nam }));
  let lam =
    ctrs.into_iter().rfold(app, |acc, arg| Term::tagged_lam(tag.clone(), Pattern::Var(Some(arg)), acc));
  ctr_args.cloned().rfold(lam, |acc, arg| Term::lam(Pattern::Var(Some(arg)), acc))
}
