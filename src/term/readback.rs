use std::{
  collections::{hash_map::Entry, HashMap},
  ops::{BitXor, Not},
};

use hvmc::ast::{Net, Tree};
use loaned::{drop, LoanedMut};

use crate::{
  diagnostics::Diagnostics,
  net::CtrKind,
  term::{num_to_name, term_to_net::Labels, Book, FanKind, Name, Pattern, Term},
};

pub fn readback(net: &Net, _: &Book, labels: &Labels, _: bool, _: &mut Diagnostics) -> Term {
  let mut root = hole();

  let mut readback = Readback {
    redexes: Default::default(),
    vars: Default::default(),
    lets: Some(&mut root),
    labels,
    name_idx: 0,
    root: None,
    garbage_terms: vec![],
    garbage_pats: vec![],
  };

  readback.readback(net);

  let Readback { garbage_terms, garbage_pats, .. } = { readback };
  drop!(LoanedMut::<Vec<Term>>::from(garbage_terms));
  drop!(LoanedMut::<Vec<Pattern>>::from(garbage_pats));

  root
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Polarity {
  Pos,
  Neg,
  Var(bool, usize),
}

impl Polarity {
  fn is_concrete(&self) -> bool {
    !matches!(self, Polarity::Var(..))
  }
}

impl Not for Polarity {
  type Output = Self;
  fn not(self) -> Self::Output {
    match self {
      Polarity::Pos => Polarity::Neg,
      Polarity::Neg => Polarity::Pos,
      Polarity::Var(inv, id) => Polarity::Var(!inv, id),
    }
  }
}

impl BitXor<bool> for Polarity {
  type Output = Self;

  fn bitxor(self, rhs: bool) -> Self::Output {
    if rhs { !self } else { self }
  }
}

#[derive(Debug)]
enum Target<'t> {
  PosTerm(LoanedMut<'t, Term>),
  NegTerm(&'t mut Term),
  PosPat(&'t mut Pattern),
  NegPat(LoanedMut<'t, Pattern>),
  NegRoot,
}

use Target::*;

impl<'t> Target<'t> {
  fn polarity(&self) -> Polarity {
    match self {
      PosTerm(_) | PosPat(_) => Polarity::Pos,
      NegTerm(_) | NegPat(_) | NegRoot => Polarity::Neg,
    }
  }
}

struct Readback<'c, 't, 'n> {
  redexes: Vec<(bool, Option<Polarity>)>,
  vars: HashMap<&'n str, Result<Target<'t>, Polarity>>,
  labels: &'c Labels,
  name_idx: u64,
  lets: Option<&'t mut Term>,
  root: Option<LoanedMut<'t, Term>>,
  garbage_terms: Vec<LoanedMut<'t, Term>>,
  garbage_pats: Vec<LoanedMut<'t, Pattern>>,
}

macro_rules! sym {
  ($a:pat, $b:pat) => {
    ($a, $b) | ($b, $a)
  };
}

impl<'c, 't, 'n> Readback<'c, 't, 'n> {
  fn readback(&mut self, net: &'n Net) {
    self.read_pos(&net.root, NegRoot);

    // dbg!(net);

    self.redexes.resize(net.redexes.len(), (false, None));
    for (i, (l, r)) in net.redexes.iter().enumerate() {
      // dbg!(i);
      let mut p = self.refine_polarity(l, Polarity::Var(false, i));
      // dbg!(p);
      // if !p.is_concrete() {
      p = !self.refine_polarity(r, !p);
      _ = p;
      // dbg!(p);
      // }
      // // dbg!(i);
      // if p.is_concrete() {
      //   self.redexes[i].0 = true;
      //   self.process_redex(p, l, r);
      // }
    }

    for (i, (l, r)) in net.redexes.iter().enumerate() {
      if self.redexes[i].0 {
        continue;
      }
      let Some(p) = self.redexes[i].1 else { todo!() };
      if !p.is_concrete() {
        todo!()
      }
      self.process_redex(p, l, r);
    }

    self.root.take().unwrap().place(self.lets.take().unwrap());
  }

  fn process_redex(&mut self, p: Polarity, l: &'n Tree, r: &'n Tree) {
    debug_assert!(p.is_concrete());
    let (p, n) = if p == Polarity::Pos { (l, r) } else { (r, l) };
    let t = self._read_pos(p);
    self.read_neg(n, t);
  }

  fn link(&mut self, a: Target<'t>, b: Target<'t>) {
    // report polarity errors
    self.equate_polarity(a.polarity(), !b.polarity());
    match (a, b) {
      sym!(PosTerm(a), NegTerm(b)) => a.place(b),
      sym!(PosPat(a), NegPat(b)) => b.place(a),
      sym!(PosPat(a), NegTerm(b)) => {
        let nam = self.gen_name();
        *a = Pattern::Var(Some(nam.clone()));
        *b = Term::Var { nam };
      }
      sym!(PosTerm(t), NegPat(p)) => self.insert_let(p, t),
      sym!(PosTerm(t), NegRoot) => self.root = Some(t),
      sym!(PosPat(p), NegRoot) => {
        let nam = self.gen_name();
        *p = Pattern::Var(Some(nam.clone()));
        self.root = Some(LoanedMut::new(Term::Var { nam }));
      }
      // push data from polarity errors to garbage:
      (NegPat(a), NegPat(b)) => {
        self.garbage_pats.push(a);
        self.garbage_pats.push(b);
      }
      sym!(NegPat(p), NegTerm(_) | NegRoot) => self.garbage_pats.push(p),
      (NegTerm(_) | NegRoot, NegTerm(_) | NegRoot) => {}
      (PosTerm(a), PosTerm(b)) => {
        self.garbage_terms.push(a);
        self.garbage_terms.push(b);
      }
      sym!(PosTerm(a), PosPat(_)) => {
        self.garbage_terms.push(a);
      }
      (PosPat(_), PosPat(_)) => {}
    }
  }

  fn insert_let(&mut self, p: LoanedMut<'t, Pattern>, v: LoanedMut<'t, Term>) {
    let lets = self.lets.take().unwrap();
    *lets = Term::Let { pat: hole(), val: hole(), nxt: hole() };
    let Term::Let { pat, val, nxt } = lets else { unreachable!() };
    self.lets = Some(nxt);
    p.place(&mut **pat);
    v.place(&mut **val);
  }

  fn collect_lets(&mut self, into: &'t mut Term, f: impl FnOnce(&mut Self)) -> &'t mut Term {
    let old_lets = std::mem::replace(&mut self.lets, Some(into));
    f(self);
    std::mem::replace(&mut self.lets, old_lets).unwrap()
  }

  fn gen_name(&mut self) -> Name {
    let nam = Name::new(num_to_name(self.name_idx));
    self.name_idx += 1;
    nam
  }

  fn equate_polarity(&mut self, a: Polarity, b: Polarity) -> Polarity {
    use Polarity::*;
    match (a, b) {
      (Pos, Pos) | (Neg, Neg) => a,
      sym!(Pos, Neg) => {
        self.report_polarity_error();
        a
      }
      _ if a == b => a,
      sym!(Var(inv, id), p) => {
        let r = &mut self.redexes[id].1;
        if let Some(q) = *r {
          self.equate_polarity(p, q ^ inv)
        } else {
          *r = Some(p ^ inv);
          p
        }
      }
    }
  }

  fn refine_polarity(&mut self, tree: &'n Tree, mut polarity: Polarity) -> Polarity {
    match tree {
      Tree::Era => polarity,
      Tree::Num { .. } | Tree::Ref { .. } => self.equate_polarity(polarity, Polarity::Pos),
      Tree::Op { rhs, out, .. } => {
        polarity = self.equate_polarity(polarity, Polarity::Neg);
        self.refine_polarity(rhs, Polarity::Pos);
        self.refine_polarity(out, Polarity::Neg);
        polarity
      }
      Tree::Mat { zero, succ, out } => {
        polarity = self.equate_polarity(polarity, Polarity::Neg);
        self.refine_polarity(zero, Polarity::Pos);
        self.refine_polarity(succ, Polarity::Pos);
        self.refine_polarity(out, Polarity::Neg);
        polarity
      }
      Tree::Ctr { lab, ports } => {
        let invert_first = match CtrKind::from_lab(*lab) {
          CtrKind::Con(_) => true,
          CtrKind::Tup(_) | CtrKind::Dup(_) => false,
        };
        for (i, port) in ports.iter().enumerate() {
          let invert = invert_first && (i != ports.len() - 1);
          // dbg!(invert);
          let out = self.refine_polarity(port, polarity ^ invert) ^ invert;
          // dbg!(invert, polarity, out, polarity ^ invert, port);
          polarity = self.equate_polarity(polarity, out);
          // if polarity.is_concrete() {
          //   break;
          // }
        }
        polarity
      }
      Tree::Var { nam } => match self.vars.entry(nam) {
        Entry::Occupied(e) => {
          let other_polarity = match e.get() {
            Ok(t) => !t.polarity(),
            Err(p) => *p,
          };
          // dbg!(nam, polarity, other_polarity);
          self.equate_polarity(polarity, !other_polarity)
        }
        Entry::Vacant(e) => {
          e.insert(Err(polarity));
          polarity
        }
      },
      Tree::Adt { .. } => unimplemented!(),
    }
  }

  fn read_pos(&mut self, tree: &'n Tree, up: Target<'t>) {
    match tree {
      Tree::Var { nam } => self.link_var(nam, up),
      _ => {
        let down = self._read_pos(tree);
        self.link(up, down);
      }
    }
  }

  fn _read_pos(&mut self, tree: &'n Tree) -> Target<'t> {
    match tree {
      Tree::Var { .. } => unreachable!(),
      Tree::Era => PosTerm(LoanedMut::new(Term::Era)),
      Tree::Num { val } => PosTerm(LoanedMut::new(Term::Num { val: *val as u64 })),
      Tree::Ref { nam } => PosTerm(LoanedMut::new(Term::Ref { nam: Name::new(nam) })),
      Tree::Ctr { lab, ports } => match CtrKind::from_lab(*lab) {
        CtrKind::Con(lab) => {
          let [fst, snd] = &ports[..] else { unimplemented!() };
          let tag = self.labels.con.to_tag(lab);
          let ((pat, bod), ret) =
            LoanedMut::loan_with(Term::Lam { tag, pat: hole(), bod: hole() }, |node, l| {
              let Term::Lam { pat, bod, .. } = node else { unreachable!() };
              (l.loan_mut(pat), l.loan_mut(bod))
            });
          let bod = self.collect_lets(bod, |slf| {
            slf.read_neg(fst, PosPat(pat));
          });
          self.read_pos(snd, NegTerm(bod));
          PosTerm(ret)
        }
        kind @ (CtrKind::Tup(_) | CtrKind::Dup(_)) => {
          let (fan, tag) = match kind {
            CtrKind::Tup(l) => (FanKind::Tup, self.labels.tup.to_tag(l)),
            CtrKind::Dup(l) => (FanKind::Dup, self.labels.dup.to_tag(Some(l))),
            CtrKind::Con(_) => unreachable!(),
          };
          let (els, ret) =
            LoanedMut::loan_with(Term::Fan { fan, tag, els: vec![hole(); ports.len()] }, |node, l| {
              let Term::Fan { els, .. } = node else { unreachable!() };
              l.loan_mut(els)
            });
          for (port, el) in ports.iter().zip(els.iter_mut()) {
            self.read_pos(port, NegTerm(el));
          }
          PosTerm(ret)
        }
      },
      Tree::Op { .. } | Tree::Mat { .. } => {
        self.report_polarity_error();
        PosTerm(hole())
      }
      Tree::Adt { .. } => unimplemented!(),
    }
  }

  fn report_polarity_error(&mut self) {
    println!("POLARITY ERROR");
  }

  fn read_neg(&mut self, tree: &'n Tree, up: Target<'t>) {
    match tree {
      Tree::Var { nam } => self.link_var(nam, up),
      Tree::Era => self.link(up, NegPat(LoanedMut::new(Pattern::Var(None)))),
      Tree::Ctr { lab, ports } => match CtrKind::from_lab(*lab) {
        CtrKind::Con(lab) => {
          let [fst, snd] = &ports[..] else { unimplemented!() };
          let tag = self.labels.con.to_tag(lab);
          let ((fun, arg), ret) =
            LoanedMut::loan_with(Term::App { tag, fun: hole(), arg: hole() }, |node, l| {
              let Term::App { fun, arg, .. } = node else { unreachable!() };
              (l.loan_mut(fun), l.loan_mut(arg))
            });
          self.link(up, NegTerm(fun));
          self.read_pos(fst, NegTerm(arg));
          self.read_neg(snd, PosTerm(ret));
        }
        kind @ (CtrKind::Tup(_) | CtrKind::Dup(_)) => {
          let (fan, tag) = match kind {
            CtrKind::Tup(l) => (FanKind::Tup, self.labels.tup.to_tag(l)),
            CtrKind::Dup(l) => (FanKind::Dup, self.labels.dup.to_tag(Some(l))),
            CtrKind::Con(_) => unreachable!(),
          };
          let (els, ret) =
            LoanedMut::loan_with(Pattern::Fan(fan, tag, vec![hole(); ports.len()]), |node, l| {
              let Pattern::Fan(.., els) = node else { unreachable!() };
              l.loan_mut(els)
            });
          self.link(up, NegPat(ret));
          for (port, el) in ports.iter().zip(els.iter_mut()) {
            self.read_neg(port, PosPat(el));
          }
        }
      },
      Tree::Op { op, rhs, out } => {
        let ((fst, snd), ret) =
          LoanedMut::loan_with(Term::Opx { opr: *op, fst: hole(), snd: hole() }, |node, l| {
            let Term::Opx { fst, snd, .. } = node else { unreachable!() };
            (l.loan_mut(fst), l.loan_mut(snd))
          });
        self.link(up, NegTerm(fst));
        self.read_pos(rhs, NegTerm(snd));
        self.read_neg(out, PosTerm(ret));
      }
      Tree::Mat { zero, succ, out } => {
        let name = self.gen_name();
        let pred_var = Name::new(format!("{name}-1"));
        let ((arg, zer, suc), swt) = LoanedMut::loan_with(
          Term::Swt {
            arg: hole(),
            bnd: Some(name.clone()),
            with: vec![],
            pred: Some(pred_var.clone()),
            arms: vec![hole(), hole()],
          },
          |node, l| {
            let Term::Swt { arg, arms, .. } = node else { unreachable!() };
            let arg = l.loan_mut(arg);
            let arms = l.loan_mut(arms);
            let (a, b) = arms.split_at_mut(1);
            (arg, &mut a[0], &mut b[0])
          },
        );
        self.link(up, NegTerm(arg));
        self.read_pos(zero, NegTerm(zer));
        self.read_neg(out, PosTerm(swt));
        if let Tree::Ctr { lab: 0, ports } = &**succ
          && let [pred, succ] = &ports[..]
        {
          let suc = self.collect_lets(suc, |slf| {
            slf.read_neg(pred, PosTerm(LoanedMut::new(Term::Var { nam: pred_var })));
          });
          self.read_pos(succ, NegTerm(suc));
        } else {
          *suc = Term::app(hole(), Term::Var { nam: pred_var });
          let Term::App { fun, .. } = suc else { unreachable!() };
          self.read_pos(succ, NegTerm(fun));
        }
      }
      Tree::Num { .. } | Tree::Ref { .. } => {
        self.report_polarity_error();
        self.link(up, NegPat(hole()));
      }
      Tree::Adt { .. } => unimplemented!(),
    }
  }

  fn link_var(&mut self, var: &'n str, x: Target<'t>) {
    match self.vars.entry(var) {
      Entry::Occupied(mut e) => match *e.get_mut() {
        Ok(_) => {
          let y = e.remove().unwrap();
          self.link(x, y);
        }
        Err(p) => {
          let q = x.polarity();
          let _ = e.insert(Ok(x));
          self.equate_polarity(p, !q);
        }
      },
      Entry::Vacant(e) => {
        e.insert(Ok(x));
      }
    }
  }
}

fn hole<T: Default>() -> T {
  T::default()
}
