use super::{Book, DefId, DefNames, Name, Op, Pattern, Tag, Term};
use crate::{
  net::{INet, NodeKind::*, Port, ROOT},
  term::MatchNum,
};
use hvmc::{
  ast::{name_to_val, val_to_name},
  run::{Loc, Val},
};
use std::collections::{hash_map::Entry, HashMap};

#[derive(Debug, Default)]
pub struct HvmcNames {
  pub id_to_hvmc_name: HashMap<DefId, Val>,
  pub hvmc_name_to_id: HashMap<Val, DefId>,
}

pub fn book_to_nets(book: &Book, main: DefId) -> (HashMap<String, INet>, HvmcNames, Labels) {
  let mut nets = HashMap::new();
  let mut hvmc_names = HvmcNames::default();
  let mut labels = Labels::default();

  for def in book.defs.values() {
    for rule in def.rules.iter() {
      let net = term_to_compat_net(&rule.body, &mut labels);

      let name = if def.def_id == main {
        DefNames::ENTRY_POINT.to_string()
      } else {
        def_id_to_hvmc_name(book, def.def_id, &nets)
      };

      let val = name_to_val(&name);
      hvmc_names.id_to_hvmc_name.insert(def.def_id, val);
      hvmc_names.hvmc_name_to_id.insert(val, def.def_id);
      nets.insert(name, net);
    }
  }

  labels.con.finish();
  labels.dup.finish();

  (nets, hvmc_names, labels)
}

/// Converts rules names to unique names compatible with hvm-core:
///   If the rule is compiler-generated: Convert the DefId value into a new name
///   If not: Truncates the rule name into 4 chars
/// Them checks if the given hashmap already contains the resulted name,
/// if it does, falls back into converting its DefId and succeeding ones until a unique name is found.
fn def_id_to_hvmc_name(book: &Book, def_id: DefId, nets: &HashMap<String, INet>) -> String {
  fn truncate(s: &str, max_chars: usize) -> &str {
    match s.char_indices().nth(max_chars) {
      None => s,
      Some((idx, _)) => &s[.. idx],
    }
  }

  fn gen_unique_name(def_id: DefId, nets: &HashMap<String, INet>) -> String {
    let name = val_to_name(def_id.to_internal());
    if nets.contains_key(&name) { gen_unique_name(DefId(def_id.0 + 1), nets) } else { name }
  }

  if book.is_generated_def(def_id) {
    gen_unique_name(def_id, nets)
  } else {
    let Name(name) = book.def_names.name(&def_id).unwrap();
    let name = truncate(name, 10);
    if !(nets.contains_key(name) || name.eq(DefNames::ENTRY_POINT)) {
      name.to_owned()
    } else {
      gen_unique_name(def_id, nets)
    }
  }
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

  let main = state.encode_term(term, ROOT);

  for (decl_port, use_port) in std::mem::take(&mut state.global_vars).into_values() {
    state.inet.link(decl_port, use_port);
  }
  if Some(ROOT) != main {
    state.link_local(ROOT, main);
  }

  state.inet
}

#[derive(Debug)]
struct EncodeTermState<'a> {
  inet: INet,
  scope: HashMap<Name, Vec<usize>>,
  vars: Vec<(Port, Option<Port>)>,
  global_vars: HashMap<Name, (Port, Port)>,
  labels: &'a mut Labels,
}

impl<'a> EncodeTermState<'a> {
  /// Adds a subterm connected to `up` to the `inet`.
  /// `scope` has the current variable scope.
  /// `vars` has the information of which ports the variables are declared and used in.
  /// `global_vars` has the same information for global lambdas. Must be linked outside this function.
  /// Expects variables to be affine, refs to be stored as Refs and all names to be bound.
  fn encode_term(&mut self, term: &Term, up: Port) -> Option<Port> {
    match term {
      // A lambda becomes to a con node. Ports:
      // - 0: points to where the lambda occurs.
      // - 1: points to the lambda variable.
      // - 2: points to the lambda body.
      // core: (var_use bod)
      Term::Lam { tag, nam, bod } => {
        let fun = self.inet.new_node(Con { lab: self.labels.con.generate(tag) });

        self.push_scope(nam, Port(fun, 1));
        let bod = self.encode_term(bod, Port(fun, 2));
        self.pop_scope(nam, Port(fun, 1));
        self.link_local(Port(fun, 2), bod);

        Some(Port(fun, 0))
      }
      // core: (var_use bod)
      Term::Chn { tag, nam, bod } => {
        let fun = self.inet.new_node(Con { lab: self.labels.con.generate(tag) });
        self.global_vars.entry(nam.clone()).or_default().0 = Port(fun, 1);
        let bod = self.encode_term(bod, Port(fun, 2));
        self.link_local(Port(fun, 2), bod);
        Some(Port(fun, 0))
      }
      // An application becomes to a con node too. Ports:
      // - 0: points to the function being applied.
      // - 1: points to the function's argument.
      // - 2: points to where the application occurs.
      // core: & fun ~ (arg ret) (fun not necessarily main port)
      Term::App { tag, fun, arg } => {
        let app = self.inet.new_node(Con { lab: self.labels.con.generate(tag) });

        let fun = self.encode_term(fun, Port(app, 0));
        self.link_local(Port(app, 0), fun);

        let arg = self.encode_term(arg, Port(app, 1));
        self.link_local(Port(app, 1), arg);

        Some(Port(app, 2))
      }
      // core: & cond ~  (zero succ) ret
      Term::Match { scrutinee, arms } => {
        let if_ = self.inet.new_node(Mat);

        let cond = self.encode_term(scrutinee, Port(if_, 0));
        self.link_local(Port(if_, 0), cond);

        debug_assert!(matches!(arms[0].0, Pattern::Num(MatchNum::Zero)));
        debug_assert!(matches!(arms[1].0, Pattern::Num(MatchNum::Succ(None))));
        let zero = &arms[0].1;
        let succ = &arms[1].1;

        let sel = self.inet.new_node(Con { lab: None });
        self.inet.link(Port(sel, 0), Port(if_, 1));
        let zero = self.encode_term(zero, Port(sel, 1));
        self.link_local(Port(sel, 1), zero);

        let succ = self.encode_term(succ, Port(sel, 2));
        self.link_local(Port(sel, 2), succ);

        Some(Port(if_, 2))
      }
      // A dup becomes a dup node too. Ports:
      // - 0: points to the value projected.
      // - 1: points to the occurrence of the first variable.
      // - 2: points to the occurrence of the second variable.
      // core: & val ~ {lab fst snd} (val not necessarily main port)
      Term::Dup { fst, snd, val, nxt, tag } => {
        let dup = self.inet.new_node(Dup { lab: self.labels.dup.generate(tag).unwrap() });

        let val = self.encode_term(val, Port(dup, 0));
        self.link_local(Port(dup, 0), val);

        self.push_scope(fst, Port(dup, 1));
        self.push_scope(snd, Port(dup, 2));
        let nxt = self.encode_term(nxt, up);
        self.pop_scope(snd, Port(dup, 2));
        self.pop_scope(fst, Port(dup, 1));

        nxt
      }
      Term::Var { nam } => {
        // We assume this variable to be valid, bound and correctly scoped.
        // This pass must be done before.
        debug_assert!(
          self.scope.contains_key(nam),
          "Unbound variable {nam}. Expected this check to be already done"
        );
        let var_stack = &self.scope[nam];
        let crnt_var = *var_stack.last().unwrap();
        let (declare_port, use_port) = self.vars.get_mut(crnt_var).unwrap();
        debug_assert!(use_port.is_none(), "Variable {nam} used more than once");
        self.inet.link(up, *declare_port);
        *use_port = Some(up);
        Some(*declare_port)
      }
      Term::Lnk { nam } => {
        self.global_vars.entry(nam.clone()).or_default().1 = up;
        None
      }
      // core: @def_id
      Term::Ref { def_id } => {
        let node = self.inet.new_node(Ref { def_id: *def_id });
        self.inet.link(Port(node, 1), Port(node, 2));
        self.inet.link(up, Port(node, 0));
        Some(Port(node, 0))
      }
      Term::Let { pat: Pattern::Tup(box Pattern::Var(l_nam), box Pattern::Var(r_nam)), val, nxt } => {
        let dup = self.inet.new_node(Tup);

        let val = self.encode_term(val, Port(dup, 0));
        self.link_local(Port(dup, 0), val);

        self.push_scope(l_nam, Port(dup, 1));
        self.push_scope(r_nam, Port(dup, 2));
        let nxt = self.encode_term(nxt, up);
        self.pop_scope(r_nam, Port(dup, 2));
        self.pop_scope(l_nam, Port(dup, 1));

        nxt
      }
      Term::Let { .. } => unreachable!(), // Removed in earlier poss
      Term::Sup { tag, fst, snd } => {
        let sup = self.inet.new_node(Dup { lab: self.labels.dup.generate(tag).unwrap() });

        let fst = self.encode_term(fst, Port(sup, 1));
        self.link_local(Port(sup, 1), fst);

        let snd = self.encode_term(snd, Port(sup, 2));
        self.link_local(Port(sup, 2), snd);

        Some(Port(sup, 0))
      }
      Term::Era => {
        let era = self.inet.new_node(Era);
        self.inet.link(Port(era, 1), Port(era, 2));
        Some(Port(era, 0))
      }
      // core: #val
      Term::Num { val } => {
        // debug_assert!(*val <= LABEL_MASK); // Uneeded?
        let node = self.inet.new_node(Num { val: *val });
        // This representation only has nodes of arity 2, so we connect the two aux ports that are not used.
        self.inet.link(Port(node, 1), Port(node, 2));
        Some(Port(node, 0))
      }
      Term::Str { .. } => unreachable!(), // Removed in desugar str
      // core: & fst ~ <op snd ret>
      Term::Opx { op, fst, snd } => {
        let opx = self.inet.new_node(Op2 { opr: op.to_hvmc_label() });

        let fst_port = self.encode_term(fst, Port(opx, 0));
        self.link_local(Port(opx, 0), fst_port);

        let snd_port = self.encode_term(snd, Port(opx, 1));
        self.link_local(Port(opx, 1), snd_port);

        Some(Port(opx, 2))
      }
      Term::Tup { fst, snd } => {
        let tup = self.inet.new_node(Tup);

        let fst = self.encode_term(fst, Port(tup, 1));
        self.link_local(Port(tup, 1), fst);

        let snd = self.encode_term(snd, Port(tup, 2));
        self.link_local(Port(tup, 2), snd);

        Some(Port(tup, 0))
      }
    }
  }

  fn push_scope(&mut self, name: &Option<Name>, decl_port: Port) {
    if let Some(name) = name {
      self.scope.entry(name.clone()).or_default().push(self.vars.len());
      self.vars.push((decl_port, None));
    }
  }

  fn pop_scope(&mut self, name: &Option<Name>, decl_port: Port) {
    if let Some(name) = name {
      self.scope.get_mut(name).unwrap().pop().unwrap();
    } else {
      let era = self.inet.new_node(Era);
      self.inet.link(decl_port, Port(era, 0));
      self.inet.link(Port(era, 1), Port(era, 2));
    }
  }

  fn link_local(&mut self, ptr_a: Port, ptr_b: Option<Port>) {
    if let Some(ptr_b) = ptr_b {
      self.inet.link(ptr_a, ptr_b);
    }
  }
}

impl Op {
  pub fn to_hvmc_label(self) -> Loc {
    match self {
      Op::ADD => 0x0,
      Op::SUB => 0x1,
      Op::MUL => 0x2,
      Op::DIV => 0x3,
      Op::MOD => 0x4,
      Op::EQ => 0x5,
      Op::NE => 0x6,
      Op::LT => 0x7,
      Op::GT => 0x8,
      Op::LTE => 0x9,
      Op::GTE => 0xa,
      Op::AND => 0xb,
      Op::OR => 0xc,
      Op::XOR => 0xd,
      Op::LSH => 0xe,
      Op::RSH => 0xf,
      Op::NOT => 0x10,
    }
  }
}

#[derive(Debug, Default)]
pub struct Labels {
  pub con: LabelGenerator,
  pub dup: LabelGenerator,
}

#[derive(Debug, Default)]
pub struct LabelGenerator {
  pub next: u32,
  pub name_to_label: HashMap<Name, u32>,
  pub label_to_name: HashMap<u32, Name>,
}

impl LabelGenerator {
  // If some tag and new generate a new label, otherwise return the generated label.
  // If none use the implicit label counter.
  fn generate(&mut self, tag: &Tag) -> Option<u32> {
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

  pub fn to_tag(&self, label: Option<u32>) -> Tag {
    match label {
      Some(label) => match self.label_to_name.get(&label) {
        Some(name) => Tag::Named(name.clone()),
        None => Tag::Numeric(label),
      },
      None => Tag::Static,
    }
  }

  fn finish(&mut self) {
    self.next = u32::MAX;
    self.name_to_label.clear();
  }
}
