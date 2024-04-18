use crate::term::{AdtEncoding, Book, Definition, Name, Pattern, Rule, Tag, Term};

impl Book {
  /// Defines a function for each constructor in each ADT in the book.
  pub fn encode_adts(&mut self, adt_encoding: AdtEncoding) {
    let mut defs = vec![];
    for (adt_name, adt) in &self.adts {
      for (ctr_name, args) in &adt.ctrs {
        let ctrs: Vec<_> = adt.ctrs.keys().cloned().collect();

        let body = encode_ctr(adt_name, args.clone(), ctrs, ctr_name, adt_encoding);

        let rules = vec![Rule { pats: vec![], body }];
        let def = Definition { name: ctr_name.clone(), rules, builtin: adt.builtin };
        defs.push((ctr_name.clone(), def));
      }
    }
    self.defs.extend(defs);
  }
}

fn encode_ctr(
  adt_name: &Name,
  ctr_args: Vec<Name>,
  ctrs: Vec<Name>,
  ctr_name: &Name,
  adt_encoding: AdtEncoding,
) -> Term {
  let tag = match adt_encoding {
    AdtEncoding::Scott => Tag::Static,
    AdtEncoding::TaggedScott => Tag::adt_name(adt_name),
  };
  let ctr = Term::Var { nam: ctr_name.clone() };
  let app =
    ctr_args.iter().cloned().fold(ctr, |acc, nam| Term::tagged_app(tag.clone(), acc, Term::Var { nam }));
  let lam =
    ctrs.into_iter().rfold(app, |acc, arg| Term::tagged_lam(tag.clone(), Pattern::Var(Some(arg)), acc));
  ctr_args.into_iter().rfold(lam, |acc, arg| Term::lam(Pattern::Var(Some(arg)), acc))
}
