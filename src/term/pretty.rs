use std::fmt::Display;

use crate::term::display::DisplayJoin;

use super::{display::DisplayFn, Name, Term};

impl Term {
  pub fn pretty(&self, tab: usize) -> impl Display + '_ {
    DisplayFn(move |f| match self {
      Term::Lam { tag, nam, bod } => {
        write!(f, "{}λ{} {}", tag.display_padded(), show_name(nam.as_ref()), bod.pretty(tab))
      }

      Term::Var { nam } => write!(f, "{nam}"),

      Term::Chn { tag, nam, bod } => {
        write!(f, "{}λ${} {}", tag, show_name(nam.as_ref()), bod.pretty(tab))
      }

      Term::Lnk { nam } => write!(f, "${nam}"),

      Term::Let { nam, val, nxt } => {
        write!(f, "let {} = {};\n{}", show_name(nam.as_ref()), val.pretty(tab), nxt.pretty(tab))
      }

      Term::Use { nam, val, nxt } => {
        write!(f, "use {} = {};\n{}", show_name(nam.as_ref()), val.pretty(tab), nxt.pretty(tab))
      }

      Term::App { tag, fun, arg } => {
        write!(f, "{}({} {})", tag.display_padded(), fun.pretty(tab), arg.pretty(tab))
      }

      Term::Ltp { bnd, val, nxt } => {
        write!(
          f,
          "let ({}) = {};\n{}",
          DisplayJoin(|| bnd.iter().map(|e| show_name(e.as_ref())), ", "),
          val.pretty(tab),
          nxt.pretty(tab),
        )
      }

      Term::Tup { els } => write!(f, "({})", DisplayJoin(|| els.iter().map(|e| e.pretty(tab)), " ")),

      Term::Dup { tag, bnd, val, nxt } => {
        write!(
          f,
          "let {}{{{}}} = {};\n{}",
          tag.display_padded(),
          DisplayJoin(|| bnd.iter().map(|e| show_name(e.as_ref())), " "),
          val.pretty(tab),
          nxt.pretty(tab),
        )
      }

      Term::Sup { tag, els } => {
        write!(f, "{}{{{}}}", tag.display_padded(), DisplayJoin(|| els.iter().map(|e| e.pretty(tab)), " "))
      }

      Term::Lst { els } => write!(f, "[{}]", DisplayJoin(|| els.iter().map(|e| e.pretty(tab)), " ")),

      Term::Opx { opr, fst, snd } => {
        write!(f, "({} {} {})", opr, fst.pretty(tab), snd.pretty(tab))
      }

      Term::Mat { arg, with, rules } => {
        let with: Box<dyn std::fmt::Display> = if with.is_empty() {
          Box::new(DisplayFn(|f| write!(f, "")))
        } else {
          Box::new(DisplayFn(|f| {
            write!(f, "with {}", DisplayJoin(|| with.iter().map(|e| e.to_string()), ", "))
          }))
        };
        write!(f, "match {} {}{{\n", arg.pretty(tab), with)?;
        for rule in rules {
          write!(
            f,
            "{:tab$}{}: {}\n",
            "",
            show_name(rule.0.as_ref()),
            rule.2.pretty(tab + 2),
            tab = tab + 2
          )?;
        }
        write!(f, "{:tab$}}}", "")?;
        Ok(())
      }

      Term::Swt { arg, with, rules } => {
        let with: Box<dyn std::fmt::Display> = if with.is_empty() {
          Box::new(DisplayFn(|f| write!(f, "")))
        } else {
          Box::new(DisplayFn(|f| {
            write!(f, "with {}", DisplayJoin(|| with.iter().map(|e| e.to_string()), ", "))
          }))
        };
        write!(f, "switch {} {}{{\n", arg.pretty(tab), with)?;
        for rule in rules {
          write!(f, "{:tab$}{}: {}\n", "", rule.0, rule.1.pretty(tab + 2), tab = tab + 2)?;
        }
        write!(f, "{:tab$}}}", "")?;
        Ok(())
      }

      Term::Nat { val } => write!(f, "#{val}"),
      Term::Num { val } => write!(f, "{val}"),
      Term::Str { val } => write!(f, "{val:?}"),
      Term::Ref { nam } => write!(f, "{nam}"),
      Term::Era => write!(f, "*"),
      Term::Err => write!(f, "<Error>"),
    })
  }
}

fn show_name(name: Option<&Name>) -> String {
  if let Some(nam) = name { nam.to_string() } else { "*".to_string() }
}
