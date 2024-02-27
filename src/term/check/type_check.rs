use crate::term::{transform::encode_pattern_matching::MatchErr, Constructors, Name, Pattern, Rule, Type};
use indexmap::IndexMap;

pub type DefinitionTypes = IndexMap<Name, Vec<Type>>;

pub fn infer_match_arg_type(rules: &[Rule], arg_idx: usize, ctrs: &Constructors) -> Result<Type, MatchErr> {
  infer_type(rules.iter().map(|r| &r.pats[arg_idx]), ctrs)
}

/// Infers the type of a sequence of arguments
pub fn infer_type<'a>(
  pats: impl IntoIterator<Item = &'a Pattern>,
  ctrs: &Constructors,
) -> Result<Type, MatchErr> {
  let mut arg_type = Type::Any;
  for pat in pats.into_iter() {
    arg_type = unify(arg_type, pat.to_type(ctrs))?;
  }
  Ok(arg_type)
}

fn unify(old: Type, new: Type) -> Result<Type, MatchErr> {
  match (old, new) {
    (Type::Any, new) => Ok(new),
    (old, Type::Any) => Ok(old),

    (Type::Adt(old), Type::Adt(new)) if new == old => Ok(Type::Adt(old)),

    (Type::Num, Type::Num) => Ok(Type::Num),
    (Type::Num, Type::NumSucc(n)) => Ok(Type::NumSucc(n)),

    (Type::NumSucc(n), Type::Num) => Ok(Type::NumSucc(n)),
    (Type::NumSucc(a), Type::NumSucc(b)) if a == b => Ok(Type::NumSucc(a)),

    (Type::Tup, Type::Tup) => Ok(Type::Tup),

    (old, new) => Err(MatchErr::TypeMismatch(new, old)),
  }
}
