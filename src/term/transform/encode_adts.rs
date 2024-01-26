use crate::term::{Book, Name, Rule, Tag, Term};

impl Book {
  pub fn generate_scott_adts(&mut self) {
    let mut defs = vec![];
    for (adt_name, adt) in &self.adts {
      for (ctr_name, args) in &adt.ctrs {
        let ctrs: Vec<_> = adt.ctrs.keys().cloned().collect();

        let lam = make_lam(adt_name, args.clone(), ctrs, ctr_name);

        let rules = vec![Rule { pats: vec![], body: lam, generated: true }];
        defs.push((ctr_name.clone(), rules));
      }
    }
    for (name, rules) in defs {
      self.insert_def(name, rules);
    }
  }
}

fn make_lam(adt_name: &Name, ctr_args: Vec<Name>, ctrs: Vec<Name>, ctr_name: &Name) -> Term {
  let ctr = Term::Var { nam: ctr_name.clone() };

  let app = ctr_args.iter().cloned().fold(ctr, |acc, nam| {
    Term::tagged_app(Tag::Named(Name(format!("{}.{}.{}", adt_name, ctr_name, nam))), acc, Term::Var { nam })
  });

  let lam =
    ctrs.into_iter().rev().fold(app, |acc, arg| Term::tagged_lam(Tag::Named(adt_name.clone()), arg, acc));

  ctr_args.into_iter().rev().fold(lam, |acc, arg| Term::named_lam(arg, acc))
}
