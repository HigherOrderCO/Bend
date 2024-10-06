use crate::{
  fun::{Book, Definition, Name, Num, Pattern, Rule, Source, Term},
  AdtEncoding,
};

impl Book {
  /// Defines a function for each constructor in each ADT in the book.
  pub fn encode_adts(&mut self, adt_encoding: AdtEncoding) {
    let mut defs = vec![];

    for (_, adt) in self.adts.iter() {
      for (ctr_idx, (ctr_name, ctr)) in adt.ctrs.iter().enumerate() {
        let ctrs: Vec<_> = adt.ctrs.keys().cloned().collect();

        let body = match adt_encoding {
          AdtEncoding::Scott => encode_ctr_scott(ctr.fields.iter().map(|f| &f.nam), ctrs, ctr_name),
          AdtEncoding::NumScott => {
            let tag = encode_num_scott_tag(ctr_idx as u32, ctr_name, adt.source.clone());
            defs.push((tag.name.clone(), tag.clone()));
            encode_ctr_num_scott(ctr.fields.iter().map(|f| &f.nam), &tag.name)
          }
        };

        let rules = vec![Rule { pats: vec![], body }];
        let def = Definition {
          name: ctr_name.clone(),
          typ: ctr.typ.clone(),
          check: true,
          rules,
          source: adt.source.clone(),
        };
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
  Term::rfold_lams(lam, ctr_args.cloned().map(Some))
}

fn encode_ctr_num_scott<'a>(ctr_args: impl DoubleEndedIterator<Item = &'a Name> + Clone, tag: &str) -> Term {
  let nam = Name::new("%x");
  // λa1 .. λan λx (x TAG a1 .. an)
  let term = Term::Var { nam: nam.clone() };
  let tag = Term::r#ref(tag);
  let term = Term::app(term, tag);
  let term = Term::call(term, ctr_args.clone().cloned().map(|nam| Term::Var { nam }));
  let term = Term::lam(Pattern::Var(Some(nam)), term);
  Term::rfold_lams(term, ctr_args.cloned().map(Some))
}

fn encode_num_scott_tag(tag : u32, ctr_name: &Name, source: Source) -> Definition {
  let tag_nam = Name::new(format!("{ctr_name}/tag"));
  let rules = vec![Rule { pats: vec![], body: Term::Num { val: Num::U24(tag) } }];
  Definition::new_gen(tag_nam.clone(), rules, source, true)
}
