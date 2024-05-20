use crate::{
  fun::{Book, Definition, Name, Num, Pattern, Rule, Term},
  AdtEncoding,
};

impl Book {
  /// Defines a function for each constructor in each ADT in the book.
  pub fn encode_adts(&mut self, adt_encoding: AdtEncoding) {
    let mut defs = vec![];
    for adt in self.adts.values() {
      for (ctr_idx, (ctr_name, fields)) in adt.ctrs.iter().enumerate() {
        let ctrs: Vec<_> = adt.ctrs.keys().cloned().collect();

        let body = match adt_encoding {
          AdtEncoding::Scott => encode_ctr_scott(fields.iter().map(|f| &f.nam), ctrs, ctr_name),
          AdtEncoding::NumScott => encode_ctr_num_scott(fields.iter().map(|f| &f.nam), ctr_idx),
        };

        let rules = vec![Rule { pats: vec![], body }];
        let def = Definition { name: ctr_name.clone(), rules, builtin: adt.builtin };
        defs.push((ctr_name.clone(), def));
      }
    }
    self.defs.extend(defs);
  }
}

fn encode_ctr_scott<'a>(
  ctr_args: impl DoubleEndedIterator<Item = &'a Name> + Clone,
  ctrs: Vec<Name>,
  ctr_name: &Name,
) -> Term {
  let ctr = Term::Var { nam: ctr_name.clone() };
  let app = Term::call(ctr, ctr_args.clone().cloned().map(|nam| Term::Var { nam }));
  let lam = Term::rfold_lams(app, ctrs.into_iter().map(Some));
  ctr_args.cloned().rfold(lam, |acc, arg| Term::lam(Pattern::Var(Some(arg)), acc))
}

fn encode_ctr_num_scott<'a>(
  ctr_args: impl DoubleEndedIterator<Item = &'a Name> + Clone,
  ctr_idx: usize,
) -> Term {
  let nam = Name::new("%x");
  // λa1 .. λan λx (x TAG a1 .. an)
  let term = Term::Var { nam: nam.clone() };
  let tag = Term::Num { val: Num::U24(ctr_idx as u32) };
  let term = Term::app(term, tag);
  let term = Term::call(term, ctr_args.clone().cloned().map(|nam| Term::Var { nam }));
  let term = Term::lam(Pattern::Var(Some(nam)), term);
  Term::rfold_lams(term, ctr_args.cloned().map(Some))
}
