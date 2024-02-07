use crate::term::{Book, DefName, Definition, Rule, Tag, TagName, Term, VarName};

impl Book {
  pub fn generate_scott_adts(&mut self) {
    let mut defs = vec![];
    for (adt_name, adt) in &self.adts {
      for (ctr_name, args) in &adt.ctrs {
        let ctrs: Vec<_> = adt.ctrs.keys().cloned().collect();

        let lam = make_lam(adt_name, args.clone(), ctrs, ctr_name);

        let rules = vec![Rule { pats: vec![], body: lam, origin: adt.origin }];
        let def = Definition { name: ctr_name.clone(), rules };
        defs.push((ctr_name.clone(), def));
      }
    }
    self.defs.extend(defs);
  }
}

fn make_lam(adt_name: &DefName, ctr_args: Vec<VarName>, ctrs: Vec<DefName>, ctr_name: &DefName) -> Term {
  let ctr = Term::Var { nam: ctr_name.clone() };

  let app = ctr_args.iter().cloned().fold(ctr, |acc, nam| {
    let tag = Tag::Named(adt_field_tag(adt_name, ctr_name, &nam));
    Term::tagged_app(tag, acc, Term::Var { nam })
  });

  let lam = ctrs
    .into_iter()
    .rev()
    .fold(app, |acc, arg| Term::tagged_lam(Tag::Named(adt_name.clone()), arg.clone(), acc));

  ctr_args.into_iter().rev().fold(lam, |acc, arg| Term::named_lam(arg, acc))
}

pub fn adt_field_tag(adt_name: &DefName, ctr_name: &DefName, field_name: &VarName) -> TagName {
  format!("{}.{}.{}", adt_name, ctr_name, field_name).into()
}
