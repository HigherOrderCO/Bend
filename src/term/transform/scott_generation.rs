use crate::term::{Book, Definition, Name, Rule, Term};

impl Book {
  pub fn generate_scott_adts(&mut self) {
    for adt in self.adts.values() {
      let curr_ctrs = adt.ctrs.clone();

      let mut ctr_args = Vec::new();
      let mut ctrs = Vec::new();

      for (ctr_name, arity) in &curr_ctrs {
        for ctr in curr_ctrs.keys() {
          ctrs.push(ctr.clone());
          if ctr == ctr_name {
            make_args(arity, &mut ctr_args);
          }
        }

        let lam = make_lam(&ctr_args, &mut ctrs, ctr_name);

        ctr_args.clear();
        ctrs.clear();

        let def_id = self.def_names.insert(ctr_name.clone());

        let rule = Rule { def_id, pats: vec![], body: lam };
        let rules = vec![rule];
        let def = Definition { def_id, rules };

        self.defs.insert(def_id, def);
      }
    }
  }
}

fn make_args(args_count: &usize, ctr_args: &mut Vec<Name>) {
  for n in 0 .. *args_count {
    let nam = Name(format!("a{n}"));
    ctr_args.push(nam);
  }
}

fn make_lam(ctr_args: &Vec<Name>, ctrs: &mut Vec<Name>, ctr_name: &Name) -> Term {
  let args_to_apply = ctr_args.clone();
  ctrs.reverse();

  let mut lam_args = ctrs.clone();
  let mut lam_args_ctrs = ctr_args.clone();
  lam_args_ctrs.reverse();
  lam_args.extend(lam_args_ctrs);

  let app = Term::Var { nam: ctr_name.clone() };

  let app = args_to_apply
    .into_iter()
    .fold(app, |acc, nam| Term::App { fun: Box::new(acc), arg: Box::new(Term::Var { nam }) });

  let lam = app;

  lam_args.into_iter().fold(lam, |acc, arg| Term::Lam { nam: Some(arg), bod: Box::new(acc) })
}

#[cfg(test)]
mod test {
  use crate::term::parser::parse_definition_book;

  #[test]
  // what should we test here?
  fn adt_generation() {
    let code = r"
    data List = (Cons x xs) | Nil
    data Bool = True | False
    data Tree = (Node val lft rgt) | Leaf
    ";
    let book = parse_definition_book(code);
    match book {
      Ok(mut book) => {
        book.generate_scott_adts();
        println!("{}", book);
      }
      Err(_) => todo!(),
    }
  }
}
