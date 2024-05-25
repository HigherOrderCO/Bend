use crate::{fun::display::DisplayFn, multi_iterator};
use hvm::ast::{Net, Tree};

pub mod add_recursive_priority;
pub mod check_net_size;
pub mod eta_reduce;
pub mod inline;
pub mod mutual_recursion;
pub mod prune;

pub fn tree_children(tree: &Tree) -> impl DoubleEndedIterator<Item = &Tree> + Clone {
  multi_iterator!(ChildrenIter { Zero, Two });
  match tree {
    Tree::Var { .. } | Tree::Ref { .. } | Tree::Era | Tree::Num { .. } => ChildrenIter::Zero([]),
    Tree::Con { fst, snd } | Tree::Dup { fst, snd } | Tree::Opr { fst, snd } | Tree::Swi { fst, snd } => {
      ChildrenIter::Two([fst.as_ref(), snd.as_ref()])
    }
  }
}

pub fn tree_children_mut(tree: &mut Tree) -> impl DoubleEndedIterator<Item = &mut Tree> {
  multi_iterator!(ChildrenIter { Zero, Two });
  match tree {
    Tree::Var { .. } | Tree::Ref { .. } | Tree::Era | Tree::Num { .. } => ChildrenIter::Zero([]),
    Tree::Con { fst, snd } | Tree::Dup { fst, snd } | Tree::Opr { fst, snd } | Tree::Swi { fst, snd } => {
      ChildrenIter::Two([fst.as_mut(), snd.as_mut()])
    }
  }
}

pub fn net_trees(net: &Net) -> impl DoubleEndedIterator<Item = &Tree> + Clone {
  [&net.root].into_iter().chain(net.rbag.iter().flat_map(|(_, fst, snd)| [fst, snd]))
}

pub fn net_trees_mut(net: &mut Net) -> impl DoubleEndedIterator<Item = &mut Tree> {
  [&mut net.root].into_iter().chain(net.rbag.iter_mut().flat_map(|(_, fst, snd)| [fst, snd]))
}

pub fn display_hvm_book(book: &hvm::ast::Book) -> impl std::fmt::Display + '_ {
  DisplayFn(|f| {
    for (nam, def) in book.defs.iter() {
      writeln!(f, "@{} = {}", nam, display_hvm_tree(&def.root))?;
      for (pri, a, b) in def.rbag.iter() {
        writeln!(f, "  &{}{} ~ {}", if *pri { "!" } else { " " }, display_hvm_tree(a), display_hvm_tree(b))?;
      }
      writeln!(f)?;
    }
    Ok(())
  })
}

// TODO: We have to reimplement these because hvm prints partially applied numbers incorrectly.
// https://github.com/HigherOrderCO/HVM/issues/350
pub fn display_hvm_numb(numb: &hvm::ast::Numb) -> impl std::fmt::Display + '_ {
  let numb = hvm::hvm::Numb(numb.0);
  match numb.get_typ() {
    hvm::hvm::TY_SYM => match numb.get_sym() as hvm::hvm::Tag {
      hvm::hvm::OP_ADD => "[+]".to_string(),
      hvm::hvm::OP_SUB => "[-]".to_string(),
      hvm::hvm::FP_SUB => "[:-]".to_string(),
      hvm::hvm::OP_MUL => "[*]".to_string(),
      hvm::hvm::OP_DIV => "[/]".to_string(),
      hvm::hvm::FP_DIV => "[:/]".to_string(),
      hvm::hvm::OP_REM => "[%]".to_string(),
      hvm::hvm::FP_REM => "[:%]".to_string(),
      hvm::hvm::OP_EQ => "[=]".to_string(),
      hvm::hvm::OP_NEQ => "[!]".to_string(),
      hvm::hvm::OP_LT => "[<]".to_string(),
      hvm::hvm::OP_GT => "[>]".to_string(),
      hvm::hvm::OP_AND => "[&]".to_string(),
      hvm::hvm::OP_OR => "[|]".to_string(),
      hvm::hvm::OP_XOR => "[^]".to_string(),
      hvm::hvm::OP_SHL => "[<<]".to_string(),
      hvm::hvm::FP_SHL => "[:<<]".to_string(),
      hvm::hvm::OP_SHR => "[>>]".to_string(),
      hvm::hvm::FP_SHR => "[:>>]".to_string(),
      _ => "[?]".to_string(),
    },
    hvm::hvm::TY_U24 => {
      let val = numb.get_u24();
      format!("{}", val)
    }
    hvm::hvm::TY_I24 => {
      let val = numb.get_i24();
      format!("{:+}", val)
    }
    hvm::hvm::TY_F24 => {
      let val = numb.get_f24();
      if val.is_infinite() {
        if val.is_sign_positive() { "+inf".to_string() } else { "-inf".to_string() }
      } else if val.is_nan() {
        "+NaN".to_string()
      } else {
        format!("{:?}", val)
      }
    }
    _ => {
      let typ = numb.get_typ();
      let val = numb.get_u24();
      format!(
        "[{}{}]",
        match typ {
          hvm::hvm::OP_ADD => "+",
          hvm::hvm::OP_SUB => "-",
          hvm::hvm::FP_SUB => ":-",
          hvm::hvm::OP_MUL => "*",
          hvm::hvm::OP_DIV => "/",
          hvm::hvm::FP_DIV => ":/",
          hvm::hvm::OP_REM => "%",
          hvm::hvm::FP_REM => ":%",
          hvm::hvm::OP_EQ => "=",
          hvm::hvm::OP_NEQ => "!",
          hvm::hvm::OP_LT => "<",
          hvm::hvm::OP_GT => ">",
          hvm::hvm::OP_AND => "&",
          hvm::hvm::OP_OR => "|",
          hvm::hvm::OP_XOR => "^",
          hvm::hvm::OP_SHL => "<<",
          hvm::hvm::FP_SHL => ":<<",
          hvm::hvm::OP_SHR => ">>",
          hvm::hvm::FP_SHR => ":>>",
          _ => "?",
        },
        val
      )
    }
  }
}

pub fn display_hvm_tree(tree: &hvm::ast::Tree) -> impl std::fmt::Display + '_ {
  match tree {
    Tree::Var { nam } => nam.to_string(),
    Tree::Ref { nam } => format!("@{}", nam),
    Tree::Era => "*".to_string(),
    Tree::Num { val } => format!("{}", display_hvm_numb(val)),
    Tree::Con { fst, snd } => format!("({} {})", display_hvm_tree(fst), display_hvm_tree(snd)),
    Tree::Dup { fst, snd } => format!("{{{} {}}}", display_hvm_tree(fst), display_hvm_tree(snd)),
    Tree::Opr { fst, snd } => format!("$({} {})", display_hvm_tree(fst), display_hvm_tree(snd)),
    Tree::Swi { fst, snd } => format!("?({} {})", display_hvm_tree(fst), display_hvm_tree(snd)),
  }
}

pub fn display_hvm_net(net: &hvm::ast::Net) -> impl std::fmt::Display + '_ {
  let mut s = display_hvm_tree(&net.root).to_string();
  for (par, fst, snd) in &net.rbag {
    s.push_str(" & ");
    s.push_str(if *par { "!" } else { "" });
    s.push_str(&display_hvm_tree(fst).to_string());
    s.push_str(" ~ ");
    s.push_str(&display_hvm_tree(snd).to_string());
  }
  s
}
