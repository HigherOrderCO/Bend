use crate::{
  maybe_grow,
  net::{
    CtrKind::*,
    INet,
    NodeKind::{self, *},
    Port, ROOT,
  },
  term::{Book, Name, Pattern, Tag, Term},
};
use std::{
  collections::{hash_map::Entry, HashMap},
  ops::{Index, IndexMut},
};

use super::FanKind;

pub fn book_to_nets(book: &Book) -> (HashMap<String, INet>, Labels) {
  let mut nets = HashMap::new();
  let mut labels = Labels::default();

  let main = book.entrypoint.as_ref().unwrap();

  for def in book.defs.values() {
    for rule in def.rules.iter() {
      let net = term_to_compat_net(&rule.body, &mut labels);

      let name = if def.name == *main { book.hvmc_entrypoint().to_string() } else { def.name.0.to_string() };

      nets.insert(name, net);
    }
  }

  labels.con.finish();
  labels.dup.finish();

  (nets, labels)
}

/// Converts an IC term into an IC net.
pub fn term_to_compat_net(term: &Term, labels: &mut Labels) -> INet {
  let mut state = EncodeTermState {
    inet: Default::default(),
    scope: Default::default(),
    vars: Default::default(),
    global_vars: Default::default(),
    labels,
  };

  state.encode_term(term, Trg::Port(ROOT));

  state.inet
}

#[derive(Debug)]
struct EncodeTermState<'a> {
  inet: INet,
  scope: HashMap<Name, Vec<Trg>>,
  global_vars: HashMap<Name, Trg>,
  labels: &'a mut Labels,
  vars: Vec<Option<Trg>>,
}

#[derive(Debug, Clone, Copy)]
enum Trg {
  Port(Port),
  Var(usize),
}

impl EncodeTermState<'_> {
  /// Adds a subterm connected to `up` to the `inet`.
  /// `scope` has the current variable scope.
  /// `vars` has the information of which ports the variables are declared and used in.
  /// `global_vars` has the same information for global lambdas. Must be linked outside this function.
  /// Expects variables to be linear, refs to be stored as Refs and all names to be bound.
  fn encode_term(&mut self, term: &Term, up: Trg) {
    maybe_grow(|| {
      match term {
        // A lambda becomes to a con node. Ports:
        // - 0: points to where the lambda occurs.
        // - 1: points to the lambda variable.
        // - 2: points to the lambda body.
        // core: (var_use bod)
        Term::Lam { tag, pat, bod } => {
          let fun = self.inet.new_node(Ctr(Con(self.labels.con.generate(tag))));
          self.link(up, Trg::Port(Port(fun, 0)));
          self.encode_pat(pat, Trg::Port(Port(fun, 1)));
          self.encode_term(bod, Trg::Port(Port(fun, 2)));
        }
        // An application becomes to a con node too. Ports:
        // - 0: points to the function being applied.
        // - 1: points to the function's argument.
        // - 2: points to where the application occurs.
        // core: & fun ~ (arg ret) (fun not necessarily main port)
        Term::App { tag, fun, arg } => {
          let app = self.inet.new_node(Ctr(Con(self.labels.con.generate(tag))));
          self.encode_term(fun, Trg::Port(Port(app, 0)));
          self.encode_term(arg, Trg::Port(Port(app, 1)));
          self.link(up, Trg::Port(Port(app, 2)));
        }
        // core: & arg ~ ?<(zero succ) ret>
        Term::Swt { arg, bnd: _, with, pred: _, arms: rules } => {
          // At this point should be only num matches of 0 and succ.
          assert!(with.is_empty());
          assert!(rules.len() == 2);

          let mat = self.inet.new_node(Mat);

          self.encode_term(arg, Trg::Port(Port(mat, 0)));

          let zero = &rules[0];
          let succ = &rules[1];

          let sel = self.inet.new_node(Ctr(Con(None)));
          self.inet.link(Port(sel, 0), Port(mat, 1));
          self.encode_term(zero, Trg::Port(Port(sel, 1)));

          self.encode_term(succ, Trg::Port(Port(sel, 2)));

          self.link(up, Trg::Port(Port(mat, 2)));
        }
        Term::Var { nam } => {
          // We assume this variable to be valid, bound and correctly scoped.
          // This pass must be done before.
          debug_assert!(
            self.scope.get(nam).is_some_and(|x| !x.is_empty()),
            "Unbound variable {nam}. Expected this check to be already done"
          );
          let down = self.scope.get_mut(nam).unwrap().pop().unwrap();
          self.link(up, down);
        }
        Term::Lnk { nam } => {
          self.link_global(nam, up);
        }
        // core: @def_id
        Term::Ref { nam: def_name } => {
          let node = self.inet.new_node(Ref { def_name: def_name.clone() });
          self.inet.link(Port(node, 1), Port(node, 2));
          self.link(up, Trg::Port(Port(node, 0)));
        }
        Term::Let { pat, val, nxt } => {
          let var = self.new_var();
          self.encode_term(val, Trg::Var(var));
          self.encode_pat(pat, Trg::Var(var));

          self.encode_term(nxt, up);
        }
        Term::Fan { fan, tag, els } => {
          let kind = self.fan_kind(fan, tag);
          self.make_node_list(
            kind,
            up,
            els.iter().map(|el| |slf: &mut Self, up| slf.encode_term(el, up)),
          );
        }
        Term::Era => {
          let era = self.inet.new_node(Era);
          self.inet.link(Port(era, 1), Port(era, 2));
          self.link(up, Trg::Port(Port(era, 0)));
        }
        // core: #val
        Term::Num { val } => {
          let node = self.inet.new_node(Num { val: *val });
          // This representation only has nodes of arity 2, so we connect the two aux ports that are not used.
          self.inet.link(Port(node, 1), Port(node, 2));
          self.link(up, Trg::Port(Port(node, 0)));
        }
        // core: & fst ~ <op snd ret>
        Term::Opx { opr, fst, snd } => {
          let opx = self.inet.new_node(Op2 { opr: *opr });
          self.encode_term(fst, Trg::Port(Port(opx, 0)));
          self.encode_term(snd, Trg::Port(Port(opx, 1)));
          self.link(up, Trg::Port(Port(opx, 2)));
        }
        Term::Use { .. }  // Removed in earlier pass
        | Term::Mat { .. } // Removed in earlier pass
        | Term::Nat { .. } // Removed in encode_nat
        | Term::Str { .. } // Removed in encode_str
        | Term::Lst { .. } // Removed in encode_list
        | Term::Err => unreachable!(),
      }
    })
  }

  fn encode_pat(&mut self, pat: &Pattern, up: Trg) {
    maybe_grow(|| match pat {
      Pattern::Var(None) => {
        let era = self.inet.new_node(Era);
        self.inet.link(Port(era, 1), Port(era, 2));
        self.link(up, Trg::Port(Port(era, 0)));
      }
      Pattern::Var(Some(name)) => self.scope.entry(name.clone()).or_default().push(up),
      Pattern::Chn(name) => self.link_global(name, up),
      Pattern::Fan(is_tup, tag, els) => {
        let kind = self.fan_kind(is_tup, tag);
        self.make_node_list(kind, up, els.iter().map(|el| |slf: &mut Self, up| slf.encode_pat(el, up)));
      }
      Pattern::Ctr(_, _) | Pattern::Num(_) | Pattern::Lst(_) | Pattern::Str(_) => unreachable!(),
    })
  }

  fn link(&mut self, x: Trg, y: Trg) {
    match (x, y) {
      (Trg::Port(p), Trg::Port(q)) => self.inet.link(p, q),
      (Trg::Var(v), t) | (t, Trg::Var(v)) => match &mut self.vars[v] {
        x @ None => *x = Some(t),
        x => {
          let u = x.take().unwrap();
          self.link(t, u);
        }
      },
    }
  }

  /// Adds a list-like tree of nodes of the same kind to the inet.
  fn make_node_list(
    &mut self,
    kind: NodeKind,
    mut up: Trg,
    mut els: impl DoubleEndedIterator<Item = impl FnOnce(&mut Self, Trg)>,
  ) {
    let last = els.next_back().unwrap();
    for item in els {
      let node = self.inet.new_node(kind.clone());
      self.link(up, Trg::Port(Port(node, 0)));
      item(self, Trg::Port(Port(node, 1)));
      up = Trg::Port(Port(node, 2));
    }
    last(self, up);
  }

  fn new_var(&mut self) -> usize {
    let i = self.vars.len();
    self.vars.push(None);
    i
  }

  fn fan_kind(&mut self, fan: &FanKind, tag: &Tag) -> NodeKind {
    let lab = self.labels[*fan].generate(tag);
    Ctr(if *fan == FanKind::Tup { Tup(lab) } else { Dup(lab.unwrap()) })
  }

  fn link_global(&mut self, name: &Name, trg: Trg) {
    match self.global_vars.entry(name.clone()) {
      Entry::Occupied(e) => {
        let other = e.remove();
        self.link(trg, other);
      }
      Entry::Vacant(e) => {
        e.insert(trg);
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
    let mut unique = || {
      let lab = self.next;
      self.next += 1;
      lab
    };
    match tag {
      Tag::Named(name) => match self.name_to_label.entry(name.clone()) {
        Entry::Occupied(e) => Some(*e.get()),
        Entry::Vacant(e) => {
          let lab = unique();
          self.label_to_name.insert(lab, name.clone());
          Some(*e.insert(lab))
        }
      },
      Tag::Numeric(lab) => Some(*lab),
      Tag::Auto => Some(unique()),
      Tag::Static => None,
    }
  }

  pub fn to_tag(&self, label: Option<u16>) -> Tag {
    match label {
      Some(label) => match self.label_to_name.get(&label) {
        Some(name) => Tag::Named(name.clone()),
        None => Tag::Numeric(label),
      },
      None => Tag::Static,
    }
  }

  fn finish(&mut self) {
    self.next = u16::MAX;
    self.name_to_label.clear();
  }
}
