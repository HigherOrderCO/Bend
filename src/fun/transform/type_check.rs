use crate::{
  diagnostics::Diagnostics,
  fun::{
    Adt, AdtCtr, Book, CtrField, Ctx, Definition, FanKind, Name, Num, Op, Pattern, Source, Tag, Term, Type,
  },
  maybe_grow,
};
use core::fmt;
use std::{collections::BTreeMap, fmt::Write, process::Output};
use TSPL::{new_parser, Parser};

impl Ctx<'_> {
  /// Type check by compiling to kind-core
  pub fn type_check(&mut self) -> Result<(), Diagnostics> {
    // Compile to kind-core
    let mut counter = 1; // Reserve 0 for unknown sources
    let mut kind_book = KindCBook::new();

    for def in self.book.defs.values() {
      if let Some(adt_nam) = self.book.ctrs.get(&def.name) {
        // Constructors get a special compilation using self types
        let adt = self.book.adts.get(adt_nam).unwrap();
        ctr_to_kindc(&def.name, adt, &mut counter, &mut kind_book);
      } else if !def.check {
        // Unchecked functions probably contain unsupported features like unscoped variables.
        // We can't check their real values, so just use an unchecked dummy.
        unchecked_def_to_kindc(&def.name, &def.typ, &mut counter, &mut kind_book);
      } else {
        // Normal functions
        def_to_kindc(def, &mut counter, self.book, &mut kind_book);
      }
    }

    // HVM native defs must be unchecked and compiled to a dummy value
    for def in self.book.hvm_defs.values() {
      unchecked_def_to_kindc(&def.name, &def.typ, &mut counter, &mut kind_book);
    }

    for adt in self.book.adts.values() {
      adt_to_kindc(adt, &mut counter, &mut kind_book);
    }

    // Build the kindc program
    let mut out = String::new();
    for (_, (nam, term)) in kind_book.iter() {
      writeln!(out, "{nam} = {term};").unwrap();
    }

    // Write the output to a temporary file
    if let Err(e) = std::fs::write(".out.kindc", out) {
      self.info.add_book_error(format!("Error writing type checking output file: {e}"));
      return self.info.fatal(());
    }

    // Call kind-core on the file
    let output = std::process::Command::new("kindc")
      .arg("check-all")
      .arg(".out.kindc")
      .output()
      .map_err(|e| format!("While running the type checker: {e}"))?;

    // Remove temporary file
    if let Err(e) = std::fs::remove_file(".out.kindc") {
      self.info.add_book_error(format!("Error removing type checking output file: {e}"));
    }

    self.read_kindc_output(output, &kind_book)?;

    self.info.fatal(())
  }

  /// Create adts and constructors for native values (numbers, tuples, etc.)
  pub fn make_native_defs(&mut self) {
    // Tuple adts
    // TODO: Make this more flexible, maybe generating only exactly the used tuple types.
    for i in 2..16 {
      let ctr_nam = Name::new(format!("kindc__tup{}/new", i));
      let vars = (0..i).map(|i| Name::new(format!("t{}", i))).collect::<Vec<_>>();
      let ctr_type = (0..i).rfold(Type::Tup(vars.iter().cloned().map(Type::Var).collect()), |typ, i| {
        Type::All(Name::new(format!("t{}", i)), Box::new(typ))
      });
      let fields = (0..i)
        .map(|i| CtrField {
          nam: Name::new(format!("x{}", i)),
          rec: false,
          typ: Type::Var(Name::new(format!("t{}", i))),
        })
        .collect();
      let ctr = AdtCtr { name: ctr_nam.clone(), typ: ctr_type, fields };

      let adt_name = Name::new(format!("kindc__tup{}", i));
      let adt = Adt {
        name: adt_name.clone(),
        vars,
        ctrs: [(ctr_nam.clone(), ctr)].into(),
        source: Source::Generated,
      };
      self.book.ctrs.insert(ctr_nam.clone(), adt_name.clone());
      self.book.adts.insert(adt_name.clone(), adt.clone());
    }
    // None adt
    let adt_name = Name::new("kindc__none");
    let ctr_nam = Name::new("kindc__none/new");
    self.book.ctrs.insert(ctr_nam.clone(), adt_name.clone());
    self.book.adts.insert(
      adt_name.clone(),
      Adt {
        name: adt_name.clone(),
        vars: vec![],
        ctrs: [(
          ctr_nam.clone(),
          AdtCtr { name: ctr_nam.clone(), typ: Type::Ctr(adt_name.clone(), vec![]), fields: vec![] },
        )]
        .into(),
        source: Source::Generated,
      },
    );
  }

  /// Read, parse and report errors from the kindc output.
  fn read_kindc_output(&mut self, output: Output, kind_book: &KindCBook) -> Result<(), Diagnostics> {
    if !output.status.success() {
      let out = String::from_utf8_lossy(&output.stdout);
      let err = String::from_utf8_lossy(&output.stderr);
      self.info.add_book_error(format!("Unexpected error from the type checker:\n{err}\n{out}"));
      return self.info.fatal(());
    }

    // Parse the output
    if !output.stderr.is_empty() {
      let err = String::from_utf8_lossy(&output.stderr);
      eprintln!("unexpected error from the type checker:\n{err}");
    } else if !output.stdout.is_empty() {
      let err = String::from_utf8_lossy(&output.stdout);
      eprintln!("raw type error:\n{err}");
      let mut p = KindCParser::new(&err);
      match p.parse_result() {
        Ok(errs) => {
          for err in errs {
            match err {
              KindCErr::ExpectedFound { expected, found, val, src } => {
                let msg = format!(
                  "Type checking error:\nExpected: {}\nFound: {}\nValue: {}\nSource: {}",
                  expected.to_bend_type(),
                  found.to_bend_type(),
                  val,
                  name_from_src(src, kind_book),
                );
                self.info.add_book_error(msg);
              }
              KindCErr::ExpectedFun { found, val, src } => {
                // TODO: Improve this message, it should mention that it's applying to a non-function.
                // Either too many args or applying to a non-function.
                let var_name = format!(
                  "Type checking error:\nExpected: function\nFound: {}\nValue: {}\nSource: {}",
                  found.to_bend_type(),
                  val,
                  name_from_src(src, kind_book),
                );
                let msg = var_name;
                self.info.add_book_error(msg);
              }
              KindCErr::ExpectedSelf { found, val, src } => {
                let msg = format!(
                  "Type checking error:\nExpected: a self-type\nFound: {}\nValue: {}\nSource: {}",
                  found.to_bend_type(),
                  val,
                  name_from_src(src, kind_book),
                );
                self.info.add_book_error(msg);
              }
              KindCErr::UndefinedRef { nam, src } => {
                let msg = format!(
                  "Type checking error:\nUndefined reference.\nValue: {}\nSource: {}",
                  nam,
                  name_from_src(src, kind_book),
                );
                self.info.add_book_error(msg);
              }
              KindCErr::CantInferLam { val, src } => {
                let msg = format!(
                  "Type checking error:\nCan't infer type of lambda.\nValue: {}\nSource: {}",
                  val,
                  name_from_src(src, kind_book),
                );
                self.info.add_book_error(msg);
              }
              KindCErr::CantInferHol { val, src } => {
                let msg = format!(
                  "Type checking error:\nCan't infer type of hole.\nValue: {}\nSource: {}",
                  val,
                  name_from_src(src, kind_book),
                );
                self.info.add_book_error(msg);
              }
              KindCErr::CantInferMet { val, src } => {
                let msg = format!(
                  "Type checking error:\nCan't infer type of meta-variable.\nValue: {}\nSource: {}",
                  val,
                  name_from_src(src, kind_book),
                );
                self.info.add_book_error(msg);
              }
              KindCErr::CantInferVar { val, src } => {
                let msg = format!(
                  "Type checking error:\nCan't infer type of variable.\nValue: {}\nSource: {}",
                  val,
                  name_from_src(src, kind_book),
                );
                self.info.add_book_error(msg);
              }
            }
          }
        }
        Err(p_err) => {
          eprintln!("Parse error: {p_err}");
          let msg = format!("Unexpected output from the type checker:\n{err}");
          self.info.add_book_error(msg);
        }
      }
    }

    self.info.fatal(())
  }
}

type KindCBook = BTreeMap<u64, (String, KindCTerm)>;

pub enum KindCTerm {
  All {
    nam: String,
    typ: Box<KindCTerm>,
    bod: Box<KindCTerm>,
  },
  Lam {
    nam: String,
    bod: Box<KindCTerm>,
  },
  App {
    fun: Box<KindCTerm>,
    arg: Box<KindCTerm>,
  },
  Ann {
    chk: bool,
    bod: Box<KindCTerm>,
    typ: Box<KindCTerm>,
  },
  Slf {
    nam: String,
    typ: Box<KindCTerm>,
    bod: Box<KindCTerm>,
  },
  Ins {
    bod: Box<KindCTerm>,
  },
  Ref {
    nam: String,
  },
  Let {
    nam: String,
    bod: Box<KindCTerm>,
    nxt: Box<KindCTerm>,
  },
  Use {
    nam: String,
    bod: Box<KindCTerm>,
    nxt: Box<KindCTerm>,
  },
  Set,
  Any,
  U48,
  I48,
  F48,
  UNum {
    val: u32,
  },
  INum {
    val: i32,
  },
  FNum {
    val: f32,
  },
  Op2 {
    op: Oper,
    fst: Box<KindCTerm>,
    snd: Box<KindCTerm>,
  },
  Swi {
    bind: String,
    arg: Box<KindCTerm>,
    zero: Box<KindCTerm>,
    succ: Box<KindCTerm>,
    motive: Box<KindCTerm>,
  },
  Hol {
    nam: String,
    ctx: Vec<KindCTerm>,
  },
  Met {
    uid: u64,
  },
  Src {
    src: u64,
    bod: Box<KindCTerm>,
  },
  Txt {
    val: String,
  },
  Nat {
    val: u32,
  },
}

use KindCTerm as KT;

pub enum Oper {
  ADD,
  SUB,
  MUL,
  DIV,
  MOD,
  EQ,
  NE,
  LT,
  GT,
  LTE,
  GTE,
  AND,
  OR,
  XOR,
  LSH,
  RSH,
}

fn def_to_kindc(def: &Definition, counter: &mut u64, bend_book: &Book, kindc_book: &mut KindCBook) {
  let src = fresh_num(counter);
  let bod = def.rule().body.to_kindc(counter, bend_book, src);
  let type_vars = def.typ.leading_type_vars();
  let bod = bod.rfold_lams(type_vars);
  let typ = def.typ.to_kindc(counter, src);
  let chk = def.check;
  let bod = KT::Ann { chk, bod: Box::new(bod), typ: Box::new(typ) };
  kindc_book.insert(src, (def.name.to_string(), bod));
}

fn ctr_to_kindc(ctr_nam: &Name, adt: &Adt, counter: &mut u64, book: &mut KindCBook) {
  // λ for each type variable
  // λ for each field
  // self instantiation
  // λ for predicate
  // λ for each constructor
  // (ctr fields) : ∀(type vars: _) ∀(field: field type) (Type vars)
  // TODO: Use the actual encoding and not always scott encoding
  let src = fresh_num(counter);
  let ctr = adt.ctrs.get(ctr_nam).unwrap();

  let term = KT::Ref { nam: ctr_nam.to_string() };
  let term = term.fold_vars(ctr.fields.iter().map(|field| field.nam.to_string()));
  let term = term.rfold_lams(adt.ctrs.keys().map(|ctr| ctr.to_string()));
  let term = term.rfold_lams(["P".to_string()]);
  let term = KT::Ins { bod: Box::new(term) };
  let term = term.rfold_lams(ctr.fields.iter().map(|field| field.nam.to_string()));
  let term = term.rfold_lams(adt.vars.iter().map(|var| var.to_string()));

  let typ = KT::Ref { nam: adt.name.to_string() };
  let typ = typ.fold_vars(adt.vars.iter().map(|var| var.to_string()));
  let typ = ctr.fields.iter().rfold(typ, |typ, field| KT::All {
    nam: field.nam.to_string(),
    typ: Box::new(field.typ.to_kindc(counter, src)),
    bod: Box::new(typ),
  });
  let typ = adt.vars.iter().rfold(typ, |typ, var| KT::All {
    nam: var.to_string(),
    typ: Box::new(KT::Set),
    bod: Box::new(typ),
  });
  let term = KT::Ann { chk: false, bod: Box::new(term), typ: Box::new(typ) };
  book.insert(src, (ctr_nam.to_string(), term));
}

fn adt_to_kindc(adt: &Adt, counter: &mut u64, book: &mut KindCBook) {
  // λ type vars
  // $(self: (Type vars))
  // ∀(P: ∀(x: (Type vars)) *)
  // ∀(constructor: ∀(fields: field types) (P (constructor vars fields)))
  // (P self): ∀(type vars: _) *
  let src = fresh_num(counter);
  let term = KT::Ref { nam: "P".to_string() };
  let term = term.fold_vars(["self".to_string()]);
  let term = adt.ctrs.iter().rfold(term, |term, (ctr_nam, ctr)| {
    let typ = KT::Ref { nam: ctr_nam.to_string() };
    let typ = typ.fold_vars(adt.vars.iter().map(|var| var.to_string()));
    let typ = typ.fold_vars(ctr.fields.iter().map(|field| field.nam.to_string()));
    let typ = KT::App { fun: Box::new(KT::Ref { nam: "P".to_string() }), arg: Box::new(typ) };
    let typ = ctr.fields.iter().rfold(typ, |typ, field| KT::All {
      nam: field.nam.to_string(),
      typ: Box::new(field.typ.to_kindc(counter, src)),
      bod: Box::new(typ),
    });
    KT::All { nam: ctr_nam.to_string(), typ: Box::new(typ), bod: Box::new(term) }
  });
  let term = KT::All {
    nam: "P".to_string(),
    typ: Box::new(KT::All {
      nam: "x".to_string(),
      typ: Box::new(
        KT::Ref { nam: adt.name.to_string() }.fold_vars(adt.vars.iter().map(|var| var.to_string())),
      ),
      bod: Box::new(KT::Set),
    }),
    bod: Box::new(term),
  };
  let term = KT::Slf {
    nam: "self".to_string(),
    typ: Box::new(
      KT::Ref { nam: adt.name.to_string() }.fold_vars(adt.vars.iter().map(|var| var.to_string())),
    ),
    bod: Box::new(term),
  };
  let term = term.rfold_lams(adt.vars.iter().map(|var| var.to_string()));

  let typ = adt.vars.iter().rfold(KT::Set, |typ, var| KT::All {
    nam: var.to_string(),
    typ: Box::new(KT::Set),
    bod: Box::new(typ),
  });

  let term = KT::Ann { chk: false, bod: Box::new(term), typ: Box::new(typ) };
  book.insert(src, (adt.name.to_string(), term));
}

fn unchecked_def_to_kindc(def_name: &Name, typ: &Type, counter: &mut u64, book: &mut KindCBook) {
  let src = fresh_num(counter);
  let term = KT::Met { uid: fresh_num(counter) };
  let typ = typ.to_kindc(counter, src);
  let term = KT::Ann { chk: false, bod: Box::new(term), typ: Box::new(typ) };
  book.insert(src, (def_name.to_string(), term));
}

impl Term {
  pub fn to_kindc(&self, counter: &mut u64, book: &Book, src: u64) -> KT {
    maybe_grow(|| {
      let term = match self {
        Term::Lam { tag, pat, bod } => {
          if tag != &Tag::Static {
            todo!();
          }
          match pat.as_ref() {
            Pattern::Var(nam) => {
              let term = KT::Lam { nam: bind_to_kind(nam), bod: Box::new(bod.to_kindc(counter, book, src)) };
              // TODO: We don't need to annotate every single lambda, only the top ones that don't already have one.
              let typ = KT::All {
                nam: "arr__".to_string(),
                typ: Box::new(KT::Met { uid: fresh_num(counter) }),
                bod: Box::new(KT::Met { uid: fresh_num(counter) }),
              };
              KT::Ann { chk: false, bod: Box::new(term), typ: Box::new(typ) }
            }
            _ => todo!(),
          }
        }
        Term::Var { nam } => KT::Ref { nam: nam.to_string() },
        Term::Let { pat, val, nxt } => match pat.as_ref() {
          Pattern::Var(nam) => KT::Let {
            nam: bind_to_kind(nam),
            bod: Box::new(val.to_kindc(counter, book, src)),
            nxt: Box::new(nxt.to_kindc(counter, book, src)),
          },
          Pattern::Fan(FanKind::Tup, Tag::Static, els) => {
            assert!(els.len() < 16);
            // First flatten the let to have a single pattern.
            let mut nxt = nxt.as_ref().clone();
            let mut binds = vec![];
            for (i, el) in els.iter().enumerate().rev() {
              if let Pattern::Var(nam) = el {
                binds.push(nam.clone());
              } else {
                let new_var = Name::new(format!("let_tup__{i}"));
                nxt = Term::Let {
                  pat: Box::new(el.clone()),
                  val: Box::new(Term::Var { nam: new_var.clone() }),
                  nxt: Box::new(nxt),
                };
                binds.push(Some(new_var));
              }
            }
            binds.reverse();

            // Then convert native tuple let terms into matches on adt tuples.
            let term = Term::Mat {
              bnd: None,
              arg: val.clone(),
              with_bnd: vec![],
              with_arg: vec![],
              arms: vec![(Some(Name::new(format!("kindc__tup{}/new", els.len()))), binds, nxt)],
            };
            term.to_kindc(counter, book, src)
          }
          Pattern::Chn(_) => unreachable!(),
          _ => todo!(),
        },
        Term::Use { nam, val, nxt } => KT::Use {
          nam: bind_to_kind(nam),
          bod: Box::new(val.to_kindc(counter, book, src)),
          nxt: Box::new(nxt.to_kindc(counter, book, src)),
        },
        Term::App { tag, fun, arg } => {
          if tag != &Tag::Static {
            todo!();
          }
          KT::App {
            fun: Box::new(fun.to_kindc(counter, book, src)),
            arg: Box::new(arg.to_kindc(counter, book, src)),
          }
        }
        Term::Num { val } => match val {
          Num::U24(val) => KT::UNum { val: *val },
          Num::I24(val) => KT::INum { val: *val },
          Num::F24(val) => KT::FNum { val: *val },
        },
        Term::Oper { opr, fst, snd } => KT::Op2 {
          op: opr.to_kindc(),
          fst: Box::new(fst.to_kindc(counter, book, src)),
          snd: Box::new(snd.to_kindc(counter, book, src)),
        },
        Term::Mat { bnd: _, arg, with_bnd: _, with_arg: _, arms } => {
          // Note: 'with' arguments should be gone by now
          // Match becomes:
          // λ type vars
          // λ P
          // λ case__ctrs
          // λ __matched
          // (~__matched P case__ctrs) :
          // ∀(type vars: _)
          // ∀(P: ∀(x: (Type vars)) *)
          // ∀(case__ctrs: ∀(fields: field types) (P (ctr vars fields)))
          // ∀(__motive: (Type vars))
          // (P __motive)
          // Then apply the matched value:
          // let arg__ = arg;
          // (__match _vars _P arms arg__)
          let ctr_nam = arms[0].0.as_ref().unwrap();
          let adt_nam = book.ctrs.get(ctr_nam).unwrap();
          let adt = book.adts.get(adt_nam).unwrap();
          let on_ctr_nams = adt.ctrs.keys().map(|ctr_nam| format!("case__{ctr_nam}"));
          let type_vars = adt.vars.iter().map(|var| var.to_string());

          // Match function
          let term = KT::Ref { nam: "matched__".to_string() };
          let term = KT::Ins { bod: Box::new(term) };
          let term = term.fold_vars(["P".to_string()]);
          let term = term.fold_vars(on_ctr_nams.clone());
          let term = term.rfold_lams(["matched__".to_string()]);
          let term = term.rfold_lams(on_ctr_nams);
          let term = term.rfold_lams(["P".to_string()]);
          let term = term.rfold_lams(type_vars.clone());

          // Type of the match function
          let typ = KT::Ref { nam: "P".to_string() };
          let typ = typ.fold_vars(["motive__".to_string()]);
          let motive_type = KT::Ref { nam: adt.name.to_string() };
          let motive_type = motive_type.fold_vars(type_vars.clone());
          let typ = KT::All { nam: "motive__".to_string(), typ: Box::new(motive_type), bod: Box::new(typ) };
          let typ = adt.ctrs.keys().rfold(typ, |typ, ctr_nam| {
            let ctr = adt.ctrs.get(ctr_nam).unwrap();
            let forall_type = KT::Ref { nam: ctr_nam.to_string() };
            let forall_type = forall_type.fold_vars(type_vars.clone());
            let forall_type = forall_type.fold_vars(ctr.fields.iter().map(|field| field.nam.to_string()));
            let forall_type =
              KT::App { fun: Box::new(KT::Ref { nam: "P".to_string() }), arg: Box::new(forall_type) };
            let forall_type = ctr.fields.iter().rfold(forall_type, |typ, field| KT::All {
              nam: field.nam.to_string(),
              typ: Box::new(field.typ.to_kindc(counter, src)),
              bod: Box::new(typ),
            });
            KT::All { nam: format!("case__{ctr_nam}"), typ: Box::new(forall_type), bod: Box::new(typ) }
          });
          let p_type = KT::All {
            nam: "x".to_string(),
            typ: Box::new(KT::Ref { nam: adt_nam.to_string() }.fold_vars(type_vars.clone())),
            bod: Box::new(KT::Set),
          };
          let typ = KT::All { nam: "P".to_string(), typ: Box::new(p_type), bod: Box::new(typ) };
          let typ = type_vars.clone().rfold(typ, |typ, var| KT::All {
            nam: var.to_string(),
            typ: Box::new(KT::Met { uid: fresh_num(counter) }),
            bod: Box::new(typ),
          });

          // Apply the arms to the match function
          let term = KT::Ann { chk: false, bod: Box::new(term), typ: Box::new(typ) };
          let term = type_vars.fold(term, |term, _| KT::App {
            fun: Box::new(term),
            arg: Box::new(KT::Met { uid: fresh_num(counter) }),
          });
          let term = KT::App { fun: Box::new(term), arg: Box::new(KT::Met { uid: fresh_num(counter) }) };
          let term = arms.iter().fold(term, |term, arm| {
            let arg = arm.2.to_kindc(counter, book, src);
            let arg =
              arm.1.iter().rfold(arg, |acc, nam| KT::Lam { nam: bind_to_kind(nam), bod: Box::new(acc) });
            KT::App { fun: Box::new(term), arg: Box::new(arg) }
          });

          // Apply the argument to the match term
          let term = KT::App { fun: Box::new(term), arg: Box::new(KT::Ref { nam: "arg__".to_string() }) };
          KT::Let {
            nam: "arg__".to_string(),
            bod: Box::new(arg.to_kindc(counter, book, src)),
            nxt: Box::new(term),
          }
        }
        Term::Swt { bnd: _, arg, with_bnd: _, with_arg: _, pred, arms } => {
          let bind = format!("swi__{}", fresh_num(counter));
          KT::Swi {
            bind: bind.clone(),
            arg: Box::new(arg.to_kindc(counter, book, src)),
            zero: Box::new(arms[0].to_kindc(counter, book, src)),
            succ: Box::new(KT::Use {
              nam: pred.as_ref().unwrap().to_string(),
              bod: Box::new(KT::Ref { nam: format!("{}-1", bind) }),
              nxt: Box::new(arms[1].to_kindc(counter, book, src)),
            }),
            motive: Box::new(KT::Met { uid: fresh_num(counter) }),
          }
        }
        Term::Ref { nam } => {
          // For function calls, we need to add a meta variable for each
          // parametric type variable of the function.
          let typ = if let Some(def) = book.defs.get(nam) {
            &def.typ
          } else if let Some(def) = book.hvm_defs.get(nam) {
            &def.typ
          } else {
            unreachable!()
          };
          typ.leading_type_vars().into_iter().fold(KT::Ref { nam: nam.to_string() }, |acc, _nam| KT::App {
            fun: Box::new(acc),
            arg: Box::new(KT::Met { uid: fresh_num(counter) }),
          })
        }

        Term::Fan { fan: FanKind::Tup, tag: Tag::Static, els } => {
          // Native tuples are converted to a tuple adt constructor.
          assert!(els.len() < 16);
          let term = Term::Ref { nam: Name::new(format!("kindc__tup{}/new", els.len())) };
          let term = els.iter().fold(term, |term, el| Term::App {
            tag: Tag::Static,
            fun: Box::new(term),
            arg: Box::new(el.clone()),
          });
          term.to_kindc(counter, book, src)
        }
        Term::Fan { fan: _, tag: _, els: _ } => todo!(),
        Term::Era => KT::Ref { nam: "kindc__none/new".to_string() },

        Term::Link { nam: _ } => unreachable!(),
        Term::With { .. } => unreachable!(),
        Term::Str { .. } => unreachable!(),
        Term::Nat { .. } => unreachable!(),
        Term::List { .. } => unreachable!(),
        Term::Fold { .. } => unreachable!(),
        Term::Bend { .. } => unreachable!(),
        Term::Open { .. } => unreachable!(),
        Term::Def { .. } => unreachable!(),
        Term::Ask { .. } => unreachable!(),
        Term::Err => unreachable!(),
      };
      KT::Src { src, bod: Box::new(term) }
    })
  }
}

impl Type {
  fn to_kindc(&self, counter: &mut u64, src: u64) -> KT {
    let kt = maybe_grow(|| match self {
      Type::Hole => KT::Met { uid: fresh_num(counter) },
      Type::Var(nam) => KT::Ref { nam: nam.to_string() },
      Type::All(nam, bod) => {
        KT::All { nam: nam.to_string(), typ: Box::new(KT::Set), bod: Box::new(bod.to_kindc(counter, src)) }
      }
      Type::Arr(lft, rgt) => KT::All {
        nam: "arr__".to_string(),
        typ: Box::new(lft.to_kindc(counter, src)),
        bod: Box::new(rgt.to_kindc(counter, src)),
      },
      Type::Ctr(nam, args) => args.iter().fold(KT::Ref { nam: nam.to_string() }, |acc, arg| KT::App {
        fun: Box::new(acc),
        arg: Box::new(arg.to_kindc(counter, src)),
      }),
      Type::U24 => KT::U48,
      // TODO: Implement i24 and f24 in kindc
      Type::I24 => KT::I48,
      Type::F24 => KT::F48,
      Type::Any => KT::Any,
      Type::Tup(els) => els.iter().fold(KT::Ref { nam: format!("kindc__tup{}", els.len()) }, |typ, el| {
        KT::App { fun: Box::new(typ), arg: Box::new(el.to_kindc(counter, src)) }
      }),
      Type::None => KT::Ref { nam: "kindc__none".to_string() },
    });
    KT::Src { src, bod: Box::new(kt) }
  }

  fn leading_type_vars(&self) -> Vec<String> {
    fn go(typ: &Type, acc: &mut Vec<String>) {
      maybe_grow(|| {
        if let Type::All(nam, bod) = typ {
          acc.push(nam.to_string());
          go(bod, acc);
        } else {
          // Not a leading type variable, stop
        }
      })
    }
    let mut vars = vec![];
    go(self, &mut vars);
    vars
  }
}

impl Op {
  pub fn to_kindc(&self) -> Oper {
    match self {
      Op::ADD => Oper::ADD,
      Op::SUB => Oper::SUB,
      Op::MUL => Oper::MUL,
      Op::DIV => Oper::DIV,
      Op::REM => Oper::MOD,
      Op::EQ => Oper::EQ,
      Op::NEQ => Oper::NE,
      Op::LT => Oper::LT,
      Op::GT => Oper::GT,
      Op::LE => Oper::LTE,
      Op::GE => Oper::GTE,
      Op::AND => Oper::AND,
      Op::OR => Oper::OR,
      Op::XOR => Oper::XOR,
      Op::SHL => Oper::LSH,
      Op::SHR => Oper::RSH,
      // TODO: Implement pow in kindc
      Op::POW => Oper::MUL,
    }
  }
}

impl KT {
  pub fn to_bend_type(&self) -> Type {
    maybe_grow(|| match self {
      // Forall types -> type variable, erase the All
      // Forall value of specific type -> function argument, convert into an arrow
      KT::All { nam: _, typ, bod } => match typ.as_ref() {
        KT::Set => bod.to_bend_type(),
        _ => Type::Arr(Box::new(typ.to_bend_type()), Box::new(bod.to_bend_type())),
      },
      KT::App { .. } => self.app_to_bend_type(vec![]),
      // TODO: This does not convert nullary constructors correctly,
      // but since we only use this for printing, it's not currently a problem.
      // Nullary constructors become variables instead.
      KT::Ref { nam } => Type::Var(Name::new(nam)),
      KT::Any => Type::Any,
      KT::U48 => Type::U24,
      KT::I48 => Type::I24,
      KT::F48 => Type::F24,
      KT::Slf { .. } => todo!(),
      KT::Ins { .. } => todo!(),
      KT::Let { .. } => todo!(),
      KT::Use { .. } => todo!(),
      KT::Set => todo!(),
      // TODO: Should use an actual unknown type here
      KT::Met { .. } => Type::Var(Name::new("_")),
      KT::Src { .. } => todo!(),
      KT::Ann { .. } => todo!(),
      KT::UNum { .. } => unreachable!(),
      KT::INum { .. } => unreachable!(),
      KT::FNum { .. } => unreachable!(),
      KT::Op2 { .. } => unreachable!(),
      KT::Swi { .. } => unreachable!(),
      KT::Hol { .. } => unreachable!(),
      KT::Txt { .. } => unreachable!(),
      KT::Lam { .. } => unreachable!(),
      KT::Nat { .. } => unreachable!(),
    })
  }

  pub fn app_to_bend_type(&self, mut args: Vec<Type>) -> Type {
    maybe_grow(|| match self {
      KT::App { fun, arg } => {
        args.push(arg.to_bend_type());
        fun.app_to_bend_type(args)
      }
      KT::Ref { nam } => {
        args.reverse();
        Type::Ctr(Name::new(nam), args)
      }
      _ => unreachable!(),
    })
  }

  pub fn fold_vars(self, vars: impl IntoIterator<Item = String>) -> Self {
    vars
      .into_iter()
      .fold(self, |acc, var| KT::App { fun: Box::new(acc), arg: Box::new(KT::Ref { nam: var }) })
  }

  pub fn rfold_lams<I>(self, lams: I) -> Self
  where
    I: IntoIterator<Item = String>,
    I::IntoIter: DoubleEndedIterator,
  {
    lams.into_iter().rfold(self, |acc, lam| KT::Lam { nam: lam, bod: Box::new(acc) })
  }
}

fn bind_to_kind(nam: &Option<Name>) -> String {
  match nam {
    Some(nam) => nam.to_string(),
    None => "era__".to_string(),
  }
}

fn fresh_num(val: &mut u64) -> u64 {
  let fresh = *val;
  *val += 1;
  fresh
}

fn name_from_src(src: u64, kind_book: &KindCBook) -> &str {
  if let Some(name) = kind_book.get(&src) {
    name.0.as_str()
  } else {
    "unknown"
  }
}
/* Stringify to KindC */

impl fmt::Display for KT {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    maybe_grow(|| match self {
      KT::All { nam, typ, bod } => write!(f, "∀({nam}: {typ}) {bod}"),
      KT::Lam { nam, bod } => write!(f, "λ{nam} {bod}"),
      KT::App { fun, arg } => write!(f, "({fun} {arg})"),
      KT::Ann { chk: false, bod, typ } => write!(f, "{{{bod}: {typ}}}"),
      KT::Ann { chk: true, bod, typ } => write!(f, "{{{bod}:: {typ}}}"),
      KT::Slf { nam, typ, bod } => write!(f, "$({nam}: {typ}) {bod}"),
      KT::Ins { bod } => write!(f, "~{bod}"),
      KT::Ref { nam } => write!(f, "{nam}"),
      KT::Let { nam, bod, nxt } => write!(f, "let {nam} = {bod}; {nxt}"),
      KT::Use { nam, bod, nxt } => write!(f, "use {nam} = {bod}; {nxt}"),
      KT::Set => write!(f, "*"),
      KT::Any => write!(f, "Any"),
      KT::U48 => write!(f, "U48"),
      KT::I48 => write!(f, "I48"),
      KT::F48 => write!(f, "F48"),
      KT::UNum { val } => write!(f, "{val}"),
      KT::INum { val } => {
        let sign = if *val >= 0 { "+" } else { "-" };
        write!(f, "{}{}", sign, val.abs())
      }
      KT::FNum { val } => {
        write!(f, "{val:.?}")
      }
      KT::Op2 { op, fst, snd } => write!(f, "({op} {fst} {snd})"),
      KT::Swi { bind, arg, zero, succ, motive } => {
        write!(f, "switch {bind} = {arg} {{ 0: {zero} _: {succ} }}: {motive}")
      }
      KT::Hol { nam, ctx } => {
        write!(f, "?{nam}")?;
        if !ctx.is_empty() {
          write!(f, " [")?;
          for (i, term) in ctx.iter().enumerate() {
            if i != 0 {
              write!(f, ", ")?;
            }
            write!(f, "{term}")?;
          }
          write!(f, "]")?;
        }
        Ok(())
      }
      KT::Met { uid } => write!(f, "_{uid}"),
      KT::Src { src, bod } => write!(f, "!{src} {bod}"),
      KT::Txt { val } => write!(f, "\"{val}\""),
      KT::Nat { val } => write!(f, "#{val}"),
    })
  }
}

impl fmt::Display for Oper {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Oper::ADD => write!(f, "+"),
      Oper::SUB => write!(f, "-"),
      Oper::MUL => write!(f, "*"),
      Oper::DIV => write!(f, "/"),
      Oper::MOD => write!(f, "%"),
      Oper::EQ => write!(f, "=="),
      Oper::NE => write!(f, "!="),
      Oper::LT => write!(f, "<"),
      Oper::GT => write!(f, ">"),
      Oper::LTE => write!(f, "<="),
      Oper::GTE => write!(f, ">="),
      Oper::AND => write!(f, "&"),
      Oper::OR => write!(f, "|"),
      Oper::XOR => write!(f, "^"),
      Oper::LSH => write!(f, "<<"),
      Oper::RSH => write!(f, ">>"),
    }
  }
}

/* Parse from KindC */
enum KindCErr {
  ExpectedFound { expected: KT, found: KT, val: KT, src: u64 },
  ExpectedFun { found: KT, val: KT, src: u64 },
  ExpectedSelf { found: KT, val: KT, src: u64 },
  UndefinedRef { nam: String, src: u64 },
  CantInferLam { val: KT, src: u64 },
  CantInferHol { val: KT, src: u64 },
  CantInferMet { val: KT, src: u64 },
  CantInferVar { val: KT, src: u64 },
}

new_parser!(KindCParser);

impl KindCParser<'_> {
  fn parse_result(&mut self) -> Result<Vec<KindCErr>, String> {
    let mut errs = vec![];
    self.skip_trivia();
    while !self.is_eof() {
      errs.push(self.parse_err()?);
      self.skip_trivia();
    }
    Ok(errs)
  }

  fn parse_err(&mut self) -> Result<KindCErr, String> {
    self.consume("#error{")?;
    self.skip_trivia();
    if self.starts_with("?function") {
      self.consume("?function")?;
      let found = self.parse_term()?;
      let val = self.parse_term()?;
      let src = self.parse_u64()?;
      self.consume("}")?;
      Ok(KindCErr::ExpectedFun { found, val, src })
    } else if self.starts_with("?self-type") {
      self.consume("?self-type")?;
      let found = self.parse_term()?;
      let val = self.parse_term()?;
      let src = self.parse_u64()?;
      self.consume("}")?;
      Ok(KindCErr::ExpectedSelf { found, val, src })
    } else if self.starts_with("?undefined_reference") {
      self.consume("?undefined_reference")?;
      self.consume("?unknown_type")?;
      let nam = self.parse_name()?;
      let src = self.parse_u64()?;
      self.consume("}")?;
      Ok(KindCErr::UndefinedRef { nam, src })
    } else if self.starts_with("?type_annotation") {
      self.consume("?type_annotation")?;
      if self.starts_with("?untyped_lambda") {
        self.consume("?untyped_lambda")?;
        let val = self.parse_term()?;
        let src = self.parse_u64()?;
        self.consume("}")?;
        Ok(KindCErr::CantInferLam { val, src })
      } else if self.starts_with("?untyped_hole") {
        self.consume("?untyped_hole")?;
        let val = self.parse_term()?;
        let src = self.parse_u64()?;
        self.consume("}")?;
        Ok(KindCErr::CantInferHol { val, src })
      } else if self.starts_with("?untyped_meta") {
        self.consume("?untyped_meta")?;
        let val = self.parse_term()?;
        let src = self.parse_u64()?;
        self.consume("}")?;
        Ok(KindCErr::CantInferMet { val, src })
      } else if self.starts_with("?untyped_variable") {
        self.consume("?untyped_variable")?;
        let val = self.parse_term()?;
        let src = self.parse_u64()?;
        self.consume("}")?;
        Ok(KindCErr::CantInferVar { val, src })
      } else {
        unreachable!()
      }
    } else {
      let expected = self.parse_term()?;
      let found = self.parse_term()?;
      let val = self.parse_term()?;
      let src = self.parse_u64()?;
      self.consume("}")?;
      Ok(KindCErr::ExpectedFound { expected, found, val, src })
    }
  }

  fn parse_term(&mut self) -> Result<KT, String> {
    maybe_grow(|| {
      self.skip_trivia();
      if self.starts_with("∀") {
        self.parse_all()
      } else if self.starts_with("λ") {
        self.parse_lam()
      } else if self.starts_with("(") {
        self.advance_one();
        self.skip_trivia();
        if let Some(c) = self.peek_one() {
          if c == '_' {
            self.parse_met()
          } else if "+-*/%=!<>&|^".contains(c) {
            self.parse_op2()
          } else {
            self.parse_app()
          }
        } else {
          self.parse_app()
        }
      } else if self.starts_with("{") {
        self.parse_ann()
      } else if self.starts_with("$(") {
        self.parse_slf()
      } else if self.starts_with("~") {
        self.parse_ins()
      } else if self.starts_with("let") {
        self.parse_let()
      } else if self.starts_with("use") {
        self.parse_use()
      } else if self.starts_with("*") {
        self.parse_set()
      } else if self.starts_with("Any") {
        self.parse_any()
      } else if self.starts_with("U48") {
        self.parse_u48()
      } else if self.starts_with("I48") {
        self.parse_i48()
      } else if self.starts_with("F48") {
        self.parse_f48()
      } else if self.starts_with("#") {
        self.parse_nat()
      } else if self.starts_with("switch") {
        self.parse_swi()
      } else if self.starts_with("\"") {
        self.parse_txt()
      } else if self.starts_with("!") {
        self.parse_src()
      } else if self.starts_with("?") {
        self.parse_hol()
      } else if let Some(c) = self.peek_one() {
        if "0123456789-+".contains(c) {
          self.parse_num()
        } else {
          self.parse_ref()
        }
      } else {
        self.parse_ref()
      }
    })
  }

  fn parse_all(&mut self) -> Result<KT, String> {
    self.consume("∀")?;
    self.consume("(")?;
    let nam = self.parse_name()?;
    self.consume(":")?;
    let typ = Box::new(self.parse_term()?);
    self.consume(")")?;
    let bod = Box::new(self.parse_term()?);
    Ok(KT::All { nam, typ, bod })
  }

  fn parse_lam(&mut self) -> Result<KT, String> {
    self.consume("λ")?;
    let nam = self.parse_name()?;
    let bod = Box::new(self.parse_term()?);
    Ok(KT::Lam { nam, bod })
  }

  fn parse_app(&mut self) -> Result<KT, String> {
    // Parens already consumed
    let fun = Box::new(self.parse_term()?);
    let arg = Box::new(self.parse_term()?);
    self.consume(")")?;
    Ok(KT::App { fun, arg })
  }

  fn parse_op2(&mut self) -> Result<KT, String> {
    // Parens already consumed
    let op = self.parse_oper()?;
    let fst = Box::new(self.parse_term()?);
    let snd = Box::new(self.parse_term()?);
    self.consume(")")?;
    Ok(KT::Op2 { op, fst, snd })
  }

  fn parse_met(&mut self) -> Result<KT, String> {
    // Parens already consumed
    self.consume("_")?;
    self.skip_trivia();
    while !self.starts_with(")") {
      // TODO: Should we do something with the spine?
      self.parse_term()?;
      self.skip_trivia();
    }
    self.consume(")")?;
    // TODO: No UID is returned, should I add something here?
    Ok(KT::Met { uid: 0 })
  }

  fn parse_ann(&mut self) -> Result<KT, String> {
    self.consume("{")?;
    let bod = Box::new(self.parse_term()?);
    self.consume(":")?;
    let chk = if self.starts_with(":") {
      self.advance_one();
      false
    } else {
      true
    };
    let typ = Box::new(self.parse_term()?);
    self.consume("}")?;
    Ok(KT::Ann { chk, bod, typ })
  }

  fn parse_slf(&mut self) -> Result<KT, String> {
    self.consume("$(")?;
    let nam = self.parse_name()?;
    self.consume(":")?;
    let typ = Box::new(self.parse_term()?);
    self.consume(")")?;
    let bod = Box::new(self.parse_term()?);
    Ok(KT::Slf { nam, typ, bod })
  }

  fn parse_ins(&mut self) -> Result<KT, String> {
    self.consume("~")?;
    let bod = Box::new(self.parse_term()?);
    Ok(KT::Ins { bod })
  }

  fn parse_let(&mut self) -> Result<KT, String> {
    self.consume("let")?;
    let nam = self.parse_name()?;
    self.consume("=")?;
    let bod = Box::new(self.parse_term()?);
    self.consume(";")?;
    let nxt = Box::new(self.parse_term()?);
    Ok(KT::Let { nam, bod, nxt })
  }

  fn parse_use(&mut self) -> Result<KT, String> {
    self.consume("use")?;
    let nam = self.parse_name()?;
    self.consume("=")?;
    let bod = Box::new(self.parse_term()?);
    self.consume(";")?;
    let nxt = Box::new(self.parse_term()?);
    Ok(KT::Use { nam, bod, nxt })
  }

  fn parse_set(&mut self) -> Result<KT, String> {
    self.consume("*")?;
    Ok(KT::Set)
  }

  fn parse_any(&mut self) -> Result<KT, String> {
    self.consume("Any")?;
    Ok(KT::Any)
  }

  fn parse_u48(&mut self) -> Result<KT, String> {
    self.consume("U48")?;
    Ok(KT::U48)
  }

  fn parse_i48(&mut self) -> Result<KT, String> {
    self.consume("I48")?;
    Ok(KT::I48)
  }

  fn parse_f48(&mut self) -> Result<KT, String> {
    self.consume("F48")?;
    Ok(KT::F48)
  }

  fn parse_num(&mut self) -> Result<KT, String> {
    let sgn = if let Some('+') = self.peek_one() {
      self.advance_one();
      Some(1)
    } else if let Some('-') = self.peek_one() {
      self.advance_one();
      Some(-1)
    } else {
      None
    };

    let int = self.parse_u64()?;

    let frac = if let Some('.') = self.peek_one() {
      self.advance_one();
      let ini_idx = *self.index();
      let frac = self.parse_u64()?;
      let end_idx = *self.index();
      let frac = frac as f32 / 10f32.powi((end_idx - ini_idx) as i32);
      Some(frac)
    } else {
      None
    };

    if let Some(frac) = frac {
      let sgn = sgn.unwrap_or(1);
      Ok(KT::FNum { val: sgn as f32 * (int as f32 + frac) })
    } else if let Some(sgn) = sgn {
      let int = sgn * int as i32;
      Ok(KT::INum { val: int })
    } else {
      Ok(KT::UNum { val: int as u32 })
    }
  }

  fn parse_swi(&mut self) -> Result<KT, String> {
    self.consume("switch")?;
    let bind = self.parse_name()?;
    self.consume("=")?;
    let arg = Box::new(self.parse_term()?);
    self.consume("{")?;
    self.consume("0:")?;
    let zero = Box::new(self.parse_term()?);
    self.consume("_:")?;
    let succ = Box::new(self.parse_term()?);
    self.consume("}")?;
    self.consume(":")?;
    let motive = Box::new(self.parse_term()?);
    Ok(KT::Swi { bind, arg, zero, succ, motive })
  }

  fn parse_txt(&mut self) -> Result<KT, String> {
    let val = self.parse_quoted_string()?;
    Ok(KT::Txt { val })
  }

  fn parse_nat(&mut self) -> Result<KT, String> {
    self.consume("#")?;
    let val = self.parse_u64()? as u32;
    Ok(KT::Nat { val })
  }

  fn parse_hol(&mut self) -> Result<KT, String> {
    self.consume("?")?;
    let nam = self.parse_name()?;
    Ok(KT::Hol { nam, ctx: vec![] })
  }

  fn parse_src(&mut self) -> Result<KT, String> {
    self.consume("!")?;
    let src = self.parse_u64()?;
    let bod = Box::new(self.parse_term()?);
    Ok(KT::Src { src, bod })
  }

  fn parse_ref(&mut self) -> Result<KT, String> {
    let nam = self.parse_name()?;
    Ok(KT::Ref { nam })
  }

  fn parse_oper(&mut self) -> Result<Oper, String> {
    let op = self.take_while(|c| "+-*/%=!<>&|^".contains(c));
    match op {
      "+" => Ok(Oper::ADD),
      "-" => Ok(Oper::SUB),
      "*" => Ok(Oper::MUL),
      "/" => Ok(Oper::DIV),
      "%" => Ok(Oper::MOD),
      "==" => Ok(Oper::EQ),
      "!=" => Ok(Oper::NE),
      "<" => Ok(Oper::LT),
      ">" => Ok(Oper::GT),
      "<=" => Ok(Oper::LTE),
      ">=" => Ok(Oper::GTE),
      "&" => Ok(Oper::AND),
      "|" => Ok(Oper::OR),
      "^" => Ok(Oper::XOR),
      "<<" => Ok(Oper::LSH),
      ">>" => Ok(Oper::RSH),
      _ => Err(format!("Invalid operator: {}", op)),
    }
  }
}
