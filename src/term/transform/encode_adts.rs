use crate::term::{Book, Name, Rule, Term};

impl Book {
  pub fn generate_scott_adts(&mut self) {
    let mut defs = vec![];
    for adt in self.adts.values() {
      for (ctr_name, args) in &adt.ctrs {
        let ctrs: Vec<_> = adt.ctrs.keys().cloned().collect();

        let lam = make_lam(args.clone(), ctrs, ctr_name);

        let rules = vec![Rule { pats: vec![], body: lam }];
        defs.push((ctr_name.clone(), rules));
      }
    }
    for (name, rules) in defs {
      self.insert_def(name, rules);
    }
  }
}

fn make_lam(ctr_args: Vec<Name>, ctrs: Vec<Name>, ctr_name: &Name) -> Term {
  let ctr = Term::Var { nam: ctr_name.clone() };

  let app = ctr_args
    .iter()
    .cloned()
    .fold(ctr, |acc, nam| Term::App { fun: Box::new(acc), arg: Box::new(Term::Var { nam }) });

  let fold_lam = |acc, arg| Term::Lam { nam: Some(arg), bod: Box::new(acc) };

  let lam = ctrs.into_iter().rev().fold(app, fold_lam);

  ctr_args.into_iter().rev().fold(lam, fold_lam)
}
