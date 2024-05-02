use crate::{
  diagnostics::Diagnostics,
  maybe_grow,
  net::CtrKind::{self, *},
  term::{Book, Name, Pattern, Tag, Term},
};
use std::{
  collections::{hash_map::Entry, HashMap},
  ops::{Index, IndexMut},
};

use hvmc::ast::{Net, Tree};
use loaned::LoanedMut;

use super::{num_to_name, FanKind, Op};

#[derive(Debug, Clone)]
pub struct ViciousCycleErr;

pub fn book_to_nets(book: &Book, diags: &mut Diagnostics) -> Result<(hvmc::ast::Book, Labels), Diagnostics> {
  diags.start_pass();

  let mut hvmc = hvmc::ast::Book::default();
  let mut labels = Labels::default();

  let main = book.entrypoint.as_ref().unwrap();

  for def in book.defs.values() {
    for rule in def.rules.iter() {
      let net = term_to_net(&rule.body, &mut labels);

      let name = if def.name == *main { book.hvmc_entrypoint().to_string() } else { def.name.0.to_string() };

      match net {
        Ok(net) => {
          hvmc.insert(name, net);
        }
        Err(err) => diags.add_inet_error(err, name),
      }
    }
  }

  labels.con.finish();
  labels.dup.finish();

  diags.fatal((hvmc, labels))
}

/// Converts an LC term into an IC net.
pub fn term_to_net(term: &Term, labels: &mut Labels) -> Result<Net, String> {
  let mut net = Net::default();

  let mut state = EncodeTermState {
    vars: Default::default(),
    wires: Default::default(),
    redexes: Default::default(),
    name_idx: 0,
    created_nodes: 0,
    labels,
  };

  state.encode_term(term, Place::Hole(&mut net.root));
  LoanedMut::from(std::mem::take(&mut state.redexes)).place(&mut net.redexes);

  let EncodeTermState { created_nodes, .. } = { state };

  let found_nodes = net.trees().map(count_nodes).sum::<usize>();
  if created_nodes != found_nodes {
    return Err("Found term that compiles into an inet with a vicious cycle".into());
  }

  Ok(net)
}

#[derive(Debug)]
struct EncodeTermState<'t, 'l> {
  vars: HashMap<(bool, Name), Place<'t>>,
  wires: Vec<Option<Place<'t>>>,
  redexes: Vec<LoanedMut<'t, (Tree, Tree)>>,
  name_idx: u64,
  created_nodes: usize,
  labels: &'l mut Labels,
}

fn count_nodes(tree: &Tree) -> usize {
  maybe_grow(|| {
    usize::from(tree.children().next().is_some()) + tree.children().map(count_nodes).sum::<usize>()
  })
}

#[derive(Debug)]
enum Place<'t> {
  Tree(LoanedMut<'t, Tree>),
  Hole(&'t mut Tree),
  Wire(usize),
}

impl<'t, 'l> EncodeTermState<'t, 'l> {
  /// Adds a subterm connected to `up` to the `inet`.
  /// `scope` has the current variable scope.
  /// `vars` has the information of which ports the variables are declared and used in.
  /// `global_vars` has the same information for global lambdas. Must be linked outside this function.
  /// Expects variables to be linear, refs to be stored as Refs and all names to be bound.
  fn encode_term(&mut self, term: &Term, up: Place<'t>) {
    maybe_grow(|| {
      match term {
        Term::Era => self.link(up, Place::Tree(LoanedMut::new(Tree::Era))),
        Term::Var { nam } => self.link_var(false, nam, up),
        Term::Lnk { nam } => self.link_var(true, nam, up),
        Term::Ref { nam } => self.link(up, Place::Tree(LoanedMut::new(Tree::Ref { nam: nam.to_string() }))),
        Term::Num { typ, val } => {
          let val = (*val << 4) | (*typ as u32);
          self.link(up, Place::Tree(LoanedMut::new(Tree::Num { val })))
        }
        // A lambda becomes to a con node. Ports:
        // - 0: points to where the lambda occurs.
        // - 1: points to the lambda variable.
        // - 2: points to the lambda body.
        // core: (var_use bod)
        Term::Lam { tag, pat, bod } => {
          let kind = Con(self.labels.con.generate(tag));
          let node = self.new_ctr(kind);
          self.link(up, node.0);
          self.encode_pat(pat, node.1);
          self.encode_term(bod, node.2);
        }
        // An application becomes to a con node too. Ports:
        // - 0: points to the function being applied.
        // - 1: points to the function's argument.
        // - 2: points to where the application occurs.
        // core: & fun ~ (arg ret) (fun not necessarily main port)
        Term::App { tag, fun, arg } => {
          let kind = Con(self.labels.con.generate(tag));
          let node = self.new_ctr(kind);
          self.encode_term(fun, node.0);
          self.encode_term(arg, node.1);
          self.link(up, node.2);
        }
        // core: & arg ~ ?<(zero succ) ret>
        Term::Swt { arg, bnd: _, with, pred: _, arms: rules } => {
          // At this point should be only num matches of 0 and succ.
          assert!(with.is_empty());
          assert!(rules.len() == 2);

          self.created_nodes += 1;
          let ((zero, succ, out), node) =
            LoanedMut::loan_with(Tree::Mat { zero: hole(), succ: hole(), out: hole() }, |t, l| {
              let Tree::Mat { zero, succ, out, .. } = t else { unreachable!() };
              (l.loan_mut(zero), l.loan_mut(succ), l.loan_mut(out))
            });

          self.encode_term(arg, Place::Tree(node));
          self.encode_term(&rules[0], Place::Hole(zero));
          self.encode_term(&rules[1], Place::Hole(succ));
          self.link(up, Place::Hole(out));
        }
        Term::Let { pat, val, nxt } => {
          let wire = self.new_wire();
          self.encode_term(val, Place::Wire(wire));
          self.encode_pat(pat, Place::Wire(wire));

          self.encode_term(nxt, up);
        }
        Term::Fan { fan, tag, els } => {
          let kind = self.fan_kind(fan, tag);
          self.make_node_list(kind, up, els.iter().map(|el| |slf: &mut Self, up| slf.encode_term(el, up)));
        }
        // core: & [opr] ~ $(fst $(snd ret))
        Term::Opr { opr, fst, snd } => {
          // Partially apply
          match (fst.as_ref(), snd.as_ref()) {
            // Put oper in fst
            (Term::Num { typ: _, val }, snd) => {
              let num_val = (*val << 4) | opr.to_native_tag();
              let fst = Place::Tree(LoanedMut::new(Tree::Num { val: num_val }));
              let node = self.new_opr();
              self.link(fst, node.0);
              self.encode_term(snd, node.1);
              self.link(up, node.2);
            }
            // Put oper in snd
            (fst, Term::Num { typ: _, val }) => {
              let num_val = (*val << 4) | opr.to_native_tag();
              let snd = Place::Tree(LoanedMut::new(Tree::Num { val: num_val }));
              let node = self.new_opr();
              self.encode_term(fst, node.0);
              self.link(snd, node.1);
              self.link(up, node.2);
            }
            // Put oper as symbol, flip with fst
            (fst, snd) => {
              let opr_val = (opr.to_native_tag() << 4) | 0x1000_0000;
              let oper = Place::Tree(LoanedMut::new(Tree::Num { val: opr_val }));
              let node1 = self.new_opr();
              self.encode_term(fst, node1.0);
              self.link(oper, node1.1);
              let node2 = self.new_opr();
              self.link(node1.2, node2.0);
              self.encode_term(snd, node2.1);
              self.link(up, node2.2);
            }
          }
        }
        Term::Use { .. }  // Removed in earlier pass
        | Term::Bnd { .. } // Removed in earlier pass
        | Term::Mat { .. } // Removed in earlier pass
        | Term::Nat { .. } // Removed in encode_nat
        | Term::Str { .. } // Removed in encode_str
        | Term::Lst { .. } // Removed in encode_list
        | Term::Err => unreachable!(),
      }
    })
  }

  fn encode_pat(&mut self, pat: &Pattern, up: Place<'t>) {
    maybe_grow(|| match pat {
      Pattern::Var(None) => self.link(up, Place::Tree(LoanedMut::new(Tree::Era))),
      Pattern::Var(Some(name)) => self.link_var(false, name, up),
      Pattern::Chn(name) => self.link_var(true, name, up),
      Pattern::Fan(fan, tag, els) => {
        let kind = self.fan_kind(fan, tag);
        self.make_node_list(kind, up, els.iter().map(|el| |slf: &mut Self, up| slf.encode_pat(el, up)));
      }
      Pattern::Ctr(_, _) | Pattern::Num(_) | Pattern::Lst(_) | Pattern::Str(_) => unreachable!(),
    })
  }

  fn link(&mut self, a: Place<'t>, b: Place<'t>) {
    match (a, b) {
      (Place::Tree(a), Place::Tree(b)) => self.redexes.push(LoanedMut::merge(Default::default(), |r, m| {
        m.place(b, &mut r.0);
        m.place(a, &mut r.1);
      })),
      (Place::Tree(t), Place::Hole(h)) | (Place::Hole(h), Place::Tree(t)) => {
        t.place(h);
      }
      (Place::Hole(a), Place::Hole(b)) => {
        let var = Tree::Var { nam: num_to_name(self.name_idx) };
        self.name_idx += 1;
        *a = var.clone();
        *b = var;
      }
      (Place::Wire(v), p) | (p, Place::Wire(v)) => {
        let v = &mut self.wires[v];
        match v.take() {
          Some(q) => self.link(p, q),
          None => *v = Some(p),
        }
      }
    }
  }

  fn new_ctr(&mut self, kind: CtrKind) -> (Place<'t>, Place<'t>, Place<'t>) {
    self.created_nodes += 1;
    let (ports, node) =
      LoanedMut::loan_with(Tree::Ctr { lab: kind.to_lab(), ports: vec![Tree::Era, Tree::Era] }, |t, l| {
        let Tree::Ctr { ports, .. } = t else { unreachable!() };
        l.loan_mut(ports)
      });
    let (a, b) = ports.split_at_mut(1);
    (Place::Tree(node), Place::Hole(&mut a[0]), Place::Hole(&mut b[0]))
  }

  fn new_opr(&mut self) -> (Place<'t>, Place<'t>, Place<'t>) {
    self.created_nodes += 1;
    let ((fst, snd), node) =
      LoanedMut::loan_with(Tree::Op { fst: Box::new(Tree::Era), snd: Box::new(Tree::Era) }, |t, l| {
        let Tree::Op { fst, snd } = t else { unreachable!() };
        (l.loan_mut(fst), l.loan_mut(snd))
      });
    (Place::Tree(node), Place::Hole(fst), Place::Hole(snd))
  }

  /// Adds a list-like tree of nodes of the same kind to the inet.
  fn make_node_list(
    &mut self,
    kind: CtrKind,
    mut up: Place<'t>,
    mut els: impl DoubleEndedIterator<Item = impl FnOnce(&mut Self, Place<'t>)>,
  ) {
    let last = els.next_back().unwrap();
    for item in els {
      let node = self.new_ctr(kind);
      self.link(up, node.0);
      item(self, node.1);
      up = node.2;
    }
    last(self, up);
  }

  fn new_wire(&mut self) -> usize {
    let i = self.wires.len();
    self.wires.push(None);
    i
  }

  fn fan_kind(&mut self, fan: &FanKind, tag: &Tag) -> CtrKind {
    let lab = self.labels[*fan].generate(tag);
    if *fan == FanKind::Tup { Tup(lab) } else { Dup(lab.unwrap()) }
  }

  fn link_var(&mut self, global: bool, name: &Name, place: Place<'t>) {
    match self.vars.entry((global, name.clone())) {
      Entry::Occupied(e) => {
        let other = e.remove();
        self.link(place, other);
      }
      Entry::Vacant(e) => {
        e.insert(place);
      }
    }
  }
}

#[derive(Debug, Default, Clone)]
pub struct Labels {
  pub con: LabelGenerator,
  pub dup: LabelGenerator,
  pub tup: LabelGenerator,
}

#[derive(Debug, Default, Clone)]
pub struct LabelGenerator {
  pub next: u16,
  pub name_to_label: HashMap<Name, u16>,
  pub label_to_name: HashMap<u16, Name>,
}

impl Index<FanKind> for Labels {
  type Output = LabelGenerator;

  fn index(&self, fan: FanKind) -> &Self::Output {
    match fan {
      FanKind::Tup => &self.tup,
      FanKind::Dup => &self.dup,
    }
  }
}

impl IndexMut<FanKind> for Labels {
  fn index_mut(&mut self, fan: FanKind) -> &mut Self::Output {
    match fan {
      FanKind::Tup => &mut self.tup,
      FanKind::Dup => &mut self.dup,
    }
  }
}

impl LabelGenerator {
  // If some tag and new generate a new label, otherwise return the generated label.
  // If none use the implicit label counter.
  fn generate(&mut self, tag: &Tag) -> Option<u16> {
    match tag {
      Tag::Named(_name) => {
        todo!("Named tags not implemented for hvm32");
        /* match self.name_to_label.entry(name.clone()) {
          Entry::Occupied(e) => Some(*e.get()),
          Entry::Vacant(e) => {
            let lab = unique();
            self.label_to_name.insert(lab, name.clone());
            Some(*e.insert(lab))
          }
        } */
      }
      Tag::Numeric(lab) => Some(*lab),
      Tag::Auto => Some(0),
      Tag::Static => None,
    }
  }

  pub fn to_tag(&self, label: Option<u16>) -> Tag {
    match label {
      Some(label) => match self.label_to_name.get(&label) {
        Some(name) => Tag::Named(name.clone()),
        None => {
          if label == 0 {
            Tag::Auto
          } else {
            Tag::Numeric(label)
          }
        }
      },
      None => Tag::Static,
    }
  }

  fn finish(&mut self) {
    self.next = u16::MAX;
    self.name_to_label.clear();
  }
}

fn hole<T: Default>() -> T {
  T::default()
}

impl Op {
  fn to_native_tag(self) -> u32 {
    match self {
      Op::ADD => 0x4,
      Op::SUB => 0x5,
      Op::MUL => 0x6,
      Op::DIV => 0x7,
      Op::REM => 0x8,
      Op::EQL => 0x9,
      Op::NEQ => 0xa,
      Op::LTN => 0xb,
      Op::GTN => 0xc,
      Op::AND => 0xd,
      Op::OR => 0xe,
      Op::XOR => 0xf,
      Op::ATN => 0xd,
      Op::LOG => 0xe,
      Op::POW => 0xf,
    }
  }
}
