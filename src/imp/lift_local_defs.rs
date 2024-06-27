use std::collections::BTreeMap;

use indexmap::IndexMap;

use crate::fun::{self, Name, Pattern};

use super::{Definition, Expr, Stmt};

impl Definition {
  pub fn lift_local_defs(&mut self, gen: &mut usize) -> Result<IndexMap<Name, fun::Definition>, String> {
    let mut defs = IndexMap::new();
    self.body.lift_local_defs(&self.name, &mut defs, gen)?;
    Ok(defs)
  }
}

impl Stmt {
  pub fn lift_local_defs(
    &mut self,
    parent: &Name,
    defs: &mut IndexMap<Name, fun::Definition>,
    gen: &mut usize,
  ) -> Result<(), String> {
    match self {
      Stmt::LocalDef { .. } => {
        let Stmt::LocalDef { mut def, mut nxt } = std::mem::take(self) else { unreachable!() };
        let children = def.lift_local_defs(gen)?;
        nxt.lift_local_defs(parent, defs, gen)?;

        let local_name = Name::new(format!("{}__local_{}_{}", parent, gen, def.name));
        *gen += 1;

        let (r#use, mut def, fvs) = gen_use(local_name.clone(), *def, nxt)?;
        *self = r#use;
        apply_closure(&mut def, fvs);

        defs.extend(children);
        defs.insert(def.name.clone(), def);
        Ok(())
      }

      Stmt::Assign { pat: _, val: _, nxt } => {
        if let Some(nxt) = nxt {
          nxt.lift_local_defs(parent, defs, gen)?;
        }
        Ok(())
      }
      Stmt::If { cond: _, then, otherwise, nxt } => {
        then.lift_local_defs(parent, defs, gen)?;
        otherwise.lift_local_defs(parent, defs, gen)?;
        if let Some(nxt) = nxt {
          nxt.lift_local_defs(parent, defs, gen)?;
        }
        Ok(())
      }
      Stmt::Match { arg: _, bnd: _, with_bnd: _, with_arg: _, arms, nxt }
      | Stmt::Fold { arg: _, bnd: _, with_bnd: _, with_arg: _, arms, nxt } => {
        for arm in arms.iter_mut() {
          arm.rgt.lift_local_defs(parent, defs, gen)?;
        }
        if let Some(nxt) = nxt {
          nxt.lift_local_defs(parent, defs, gen)?;
        }
        Ok(())
      }
      Stmt::Switch { arg: _, bnd: _, with_bnd: _, with_arg: _, arms, nxt } => {
        for arm in arms.iter_mut() {
          arm.lift_local_defs(parent, defs, gen)?;
        }
        if let Some(nxt) = nxt {
          nxt.lift_local_defs(parent, defs, gen)?;
        }
        Ok(())
      }
      Stmt::Bend { bnd: _, arg: _, cond: _, step, base, nxt } => {
        step.lift_local_defs(parent, defs, gen)?;
        base.lift_local_defs(parent, defs, gen)?;
        if let Some(nxt) = nxt {
          nxt.lift_local_defs(parent, defs, gen)?;
        }
        Ok(())
      }
      Stmt::With { typ: _, bod, nxt } => {
        bod.lift_local_defs(parent, defs, gen)?;
        if let Some(nxt) = nxt {
          nxt.lift_local_defs(parent, defs, gen)?;
        }
        Ok(())
      }

      Stmt::InPlace { op: _, pat: _, val: _, nxt }
      | Stmt::Ask { pat: _, val: _, nxt }
      | Stmt::Open { typ: _, var: _, nxt }
      | Stmt::Use { nam: _, val: _, nxt } => nxt.lift_local_defs(parent, defs, gen),

      Stmt::Return { .. } | Stmt::Err => Ok(()),
    }
  }
}

fn gen_use(
  local_name: Name,
  def: Definition,
  nxt: Box<Stmt>,
) -> Result<(Stmt, fun::Definition, Vec<Name>), String> {
  let params = def.params.clone();
  let mut def = def.to_fun(false)?;

  let fvs = BTreeMap::from_iter(def.rules[0].body.free_vars());
  let fvs = fvs.into_keys().filter(|fv| !params.contains(fv)).collect::<Vec<_>>();
  let val = Expr::Call {
    fun: Box::new(Expr::Var { nam: local_name.clone() }),
    args: fvs.iter().cloned().map(|nam| Expr::Var { nam }).collect(),
    kwargs: vec![],
  };

  let r#use = Stmt::Use { nam: def.name.clone(), val: Box::new(val), nxt };
  def.name = local_name;

  Ok((r#use, def, fvs))
}

fn apply_closure(def: &mut fun::Definition, fvs: Vec<Name>) {
  let rule = &mut def.rules[0];
  let mut n_pats = fvs.into_iter().map(|x| Pattern::Var(Some(x))).collect::<Vec<_>>();
  let rule_pats = std::mem::take(&mut rule.pats);
  n_pats.extend(rule_pats);
  rule.pats = n_pats;
}
