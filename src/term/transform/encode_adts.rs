use crate::term::{AdtEncoding, Book, Definition, Name, Rule, Tag, Term};

impl Book {
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
  match adt_encoding {
    // λarg1 λarg2 λarg3 λctr1 λctr2 (ctr2 arg1 arg2 arg3)
    AdtEncoding::Scott => {
      let ctr = Term::Var { nam: ctr_name.clone() };
      let app = ctr_args.iter().cloned().fold(ctr, Term::arg_call);
      let lam = ctrs.into_iter().rev().fold(app, |acc, arg| Term::named_lam(arg.clone(), acc));
      ctr_args.into_iter().rev().fold(lam, |acc, arg| Term::named_lam(arg, acc))
    }
    // λarg1 λarg2 #type λctr1 #type λctr2 #type.ctr2.arg2(#type.ctr2.arg1(ctr2 arg1) arg2)
    AdtEncoding::TaggedScott => {
      let ctr = Term::Var { nam: ctr_name.clone() };
      let app = ctr_args.iter().cloned().fold(ctr, |acc, nam| {
        let tag = Tag::adt_name(adt_name);
        Term::tagged_app(tag, acc, Term::Var { nam })
      });
      let lam = ctrs
        .into_iter()
        .rev()
        .fold(app, |acc, arg| Term::tagged_lam(Tag::adt_name(adt_name), arg.clone(), acc));
      ctr_args.into_iter().rev().fold(lam, |acc, arg| Term::named_lam(arg, acc))
    }
  }
}
