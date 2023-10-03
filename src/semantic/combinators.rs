use crate::ast::{hvm_lang::DefNames, Definition, DefinitionBook, Name, Rule, Term};

/// Replaces closed Terms (i.e. without free variables) with a Ref to the extracted term
/// Precondition: Vars must have been sanitized
impl DefinitionBook {
  pub fn detach_combinators(&mut self) {
    let mut comb = self.register_combinators();

    for def in self.defs.iter_mut() {
      for rule in def.rules.iter_mut() {
        // println!("Term:\n{}\n", rule.body.to_string(&self.def_names));
        rule.body.abstract_lambdas(&self.def_names);
        // println!("Result:\n{}\n", rule.body.to_string(&self.def_names));
      }
    }

    self.defs.append(&mut comb);
  }
}

impl Term {
  pub fn abstract_lambdas(&mut self, names: &DefNames) {
    match self {
      Term::Lam { nam: Some(_), bod: _ } => self.abstract_lambda(names),
      Term::Lam { nam: _, bod } => bod.abstract_lambdas(names),
      Term::Chn { nam: _, bod } => bod.abstract_lambdas(names),
      Term::Let { nam: _, val, nxt } => {
        val.abstract_lambdas(names);
        nxt.abstract_lambdas(names);
      }
      Term::App { fun, arg } => {
        fun.abstract_lambdas(names);
        arg.abstract_lambdas(names);
      }
      Term::Dup { fst: _, snd: _, val, nxt } => {
        val.abstract_lambdas(names);
        nxt.abstract_lambdas(names);
      }
      Term::Sup { .. } => todo!(),
      Term::Var { .. } => {}
      Term::Lnk { .. } => {}
      Term::Ref { .. } => {}
      Term::Era => {}
    }
  }
}

#[derive(Debug)]
pub enum Combinator {
  K(Term),
  KC(Box<Combinator>),
  I,
  T(Term),
  TC(Box<Combinator>),
  B_(Term, Term, Box<Combinator>),
  C_(Term, Box<Combinator>, Term),
  S_(Term, Box<Combinator>, Box<Combinator>),
  B(Term, Box<Combinator>),
  C(Box<Combinator>, Term),
  S(Box<Combinator>, Box<Combinator>),
}

impl Combinator {
  pub fn to_string(&self, names: &DefNames) -> String {
    match self {
      Self::K(term) => term.to_string(names),
      Self::KC(comb) => comb.to_string(names),
      Self::I => "I".to_owned(),
      Self::T(term) => term.to_string(names),
      Self::TC(comb) => comb.to_string(names),
      Self::B_(e, t, c) => {
        format!("(B_ {} {} {})", e.to_string(names), t.to_string(names), c.to_string(names))
      }
      Self::C_(e, c, t) => {
        format!("(C_ {} {} {})", e.to_string(names), c.to_string(names), t.to_string(names))
      }
      Self::S_(e, c1, c2) => {
        format!("(S_ {} {} {})", e.to_string(names), c1.to_string(names), c2.to_string(names))
      }
      Self::B(t, c) => format!("(B {} {})", t.to_string(names), c.to_string(names)),
      Self::C(c, t) => format!("(C {} {})", c.to_string(names), t.to_string(names)),
      Self::S(c1, c2) => format!("(S {} {})", c1.to_string(names), c2.to_string(names)),
    }
  }

  pub fn to_term(self, names: &DefNames) -> Term {
    match self {
      Self::K(t) => Term::app(comb_ref("_K", names), t),
      Self::KC(c) => Term::app(comb_ref("_K", names), c.to_term(names)),
      Self::I => comb_ref("_I", names),
      Self::T(t) => t,
      Self::TC(c) => c.to_term(names),
      Self::B_(d, t, c) => Term::call(comb_ref("_B_", names), [d, t, c.to_term(names)]),
      Self::C_(d, c, t) => Term::call(comb_ref("_C_", names), [d, c.to_term(names), t]),
      Self::S_(d, c1, c2) => Term::call(comb_ref("_S_", names), [d, c1.to_term(names), c2.to_term(names)]),
      Self::B(t, c) => Term::call(comb_ref("_B", names), [t, c.to_term(names)]),
      Self::C(c, t) => Term::call(comb_ref("_C", names), [c.to_term(names), t]),
      Self::S(c1, c2) => Term::call(comb_ref("_S", names), [c1.to_term(names), c2.to_term(names)]),
    }
  }
}

fn comb_ref(name: &str, names: &DefNames) -> Term {
  let def_id = names.def_id(&Name::new(name)).unwrap();
  Term::Ref { def_id }
}

impl Term {
  pub fn abstract_lambda(&mut self, names: &DefNames) {
    let extracted = std::mem::replace(self, Term::Era);
    let Self::Lam { nam: Some(ref name), bod } = extracted else { panic!() };

    *self = bod.abstract_by(&name, names).to_term(names);
  }

  /// abcdef algorithm - pp. 42–67 of the second volume of Combinatory Logic
  pub fn abstract_by(mut self, name: &str, names: &DefNames) -> Combinator {
    use Combinator as C;

    match self {
      // Especial case
      Self::Lam { nam: Some(_), .. } => {
        self.abstract_lambda(names);
        C::TC(Box::new(self.abstract_by(name, names)))
      }

      // Especial case
      Self::Lam { nam: None, bod } => C::KC(Box::new(bod.abstract_by(name, names))),

      // (a)
      _ if !self.occours_check(name) => C::K(self),

      // (b)
      Self::Var { .. } => C::I,

      // (c)
      Self::App { fun, arg } if arg.is_var(name) => C::T(*fun),

      Self::App { fun: box Self::App { fun, arg }, arg: arg2 } if !fun.occours_check(name) => {
        match (arg.occours_check(name), arg2.occours_check(name)) {
          (true, true) => {
            C::S_(*fun, Box::new(arg.abstract_by(name, names)), Box::new(arg2.abstract_by(name, names)))
          }
          (true, false) => C::C_(*fun, Box::new(arg.abstract_by(name, names)), *arg2),
          (false, true) => C::B_(*fun, *arg, Box::new(arg2.abstract_by(name, names))),
          (false, false) => unreachable!(),
        }
      }

      // (d)
      Self::App { fun, arg } if !fun.occours_check(name) && arg.occours_check(name) => {
        C::B(*fun, Box::new(arg.abstract_by(name, names)))
      }

      // (e)
      Self::App { fun, arg } if fun.occours_check(name) && !arg.occours_check(name) => {
        C::C(Box::new(fun.abstract_by(name, names)), *arg)
      }

      // (f)
      Self::App { fun, arg } if fun.occours_check(name) && arg.occours_check(name) => {
        C::S(Box::new(fun.abstract_by(name, names)), Box::new(arg.abstract_by(name, names)))
      }

      Self::App { .. } => unreachable!(),
      Self::Ref { .. } => unreachable!(),
      Self::Lnk { .. } => unreachable!(),
      Self::Era => unreachable!(),

      Self::Chn { .. } => todo!(),
      Self::Let { .. } => todo!(),
      Self::Dup { .. } => todo!(),
      Self::Sup { .. } => todo!(),
    }
  }

  pub fn occours_check(&self, name: &str) -> bool {
    match self {
      Term::Var { nam: Name(n) } => n == name,
      Term::Lam { nam, bod } => !nam.as_ref().is_some_and(|Name(n)| n == name) && bod.occours_check(name),
      Term::Chn { nam: _, bod } => bod.occours_check(name),
      Term::App { fun, arg } => fun.occours_check(name) || arg.occours_check(name),
      Term::Sup { fst, snd } => fst.occours_check(name) || snd.occours_check(name),
      Term::Let { nam: Name(n), val, nxt } => {
        val.occours_check(name) || (n != name && nxt.occours_check(name))
      }
      Term::Dup { fst, snd, val, nxt } => {
        val.occours_check(name)
          || (!fst.as_ref().is_some_and(|Name(n)| n == name)
            && !snd.as_ref().is_some_and(|Name(n)| n == name)
            && nxt.occours_check(name))
      }
      Term::Lnk { .. } => false,
      Term::Ref { .. } => false,
      Term::Era => false,
    }
  }

  fn is_var(&self, name: &str) -> bool {
    matches!(self, Term::Var { nam: Name(n) } if n == name)
  }

  fn lam(name: Option<&str>, body: Self) -> Self {
    Self::Lam { nam: name.map(Name::new), bod: Box::new(body) }
  }

  fn app(fun: Self, arg: Self) -> Self {
    Self::App { fun: Box::new(fun), arg: Box::new(arg) }
  }

  fn var(name: &str) -> Self {
    Self::Var { nam: Name::new(name) }
  }
}

impl DefinitionBook {
  pub fn register_combinators(&mut self) -> Vec<Definition> {
    let mut combinators = Vec::new();

    combinators.push(self.register("_K", Term::lam(Some("x"), Term::lam(Some("y"), Term::var("x")))));
    combinators.push(self.register("_I", Term::lam(Some("x"), Term::var("x"))));

    let xyz_lambda = |body| Term::lam(Some("x"), Term::lam(Some("y"), Term::lam(Some("z"), body)));
    let dxyz_lambda = |body| {
      Term::lam(
        Some("d"),
        xyz_lambda({
          let Term::App { fun, arg } = body else { panic!() };
          Term::App { fun: Box::new(Term::App { fun: Box::new(Term::var("d")), arg: fun }), arg }
        }),
      )
    };

    let b_body = Term::app(Term::var("x"), Term::app(Term::var("y"), Term::var("z")));

    combinators.push(self.register("_B", xyz_lambda(b_body.clone())));
    combinators.push(self.register("_B_", dxyz_lambda(b_body)));

    let c_body = Term::app(Term::app(Term::var("x"), Term::var("z")), Term::var("y"));

    combinators.push(self.register("_C", xyz_lambda(c_body.clone())));
    combinators.push(self.register("_C_", dxyz_lambda(c_body)));

    let s_body =
      Term::app(Term::app(Term::var("x"), Term::var("z")), Term::app(Term::var("y"), Term::var("z")));

    combinators.push(self.register("_S", xyz_lambda(s_body.clone())));
    combinators.push(self.register("_S_", dxyz_lambda(s_body)));

    combinators
  }

  pub fn register(&mut self, name: &str, body: Term) -> Definition {
    let def_id = self.def_names.insert(Name::new(name));
    Definition { def_id, rules: vec![Rule { def_id, pats: Vec::new(), body }] }
  }
}

#[cfg(test)]
mod tests {
  use crate::ast::{DefinitionBook, Term};

  #[test]
  fn test() {
    let mut book = DefinitionBook::new();
    book.register_combinators();

    let mut test_case = Term::lam(
      Some("f"),
      Term::lam(
        Some("g"),
        Term::lam(
          Some("x"),
          Term::app(
            Term::app(Term::var("some_var"), Term::app(Term::var("f"), Term::var("x"))),
            Term::app(Term::var("g"), Term::var("x")),
          ),
        ),
      ),
    );

    println!("Term:\n{}\n", test_case.to_string(&book.def_names));
    test_case.abstract_lambdas(&book.def_names);
    println!("Result:\n{}\n", test_case.to_string(&book.def_names));
  }
}
