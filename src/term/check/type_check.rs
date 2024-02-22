use crate::term::{transform::encode_pattern_matching::MatchErr, Constructors, Name, Pattern, Type};
use indexmap::IndexMap;

pub type DefinitionTypes = IndexMap<Name, Vec<Type>>;

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
    (Type::Tup, Type::Tup) => Ok(Type::Tup),
    (old, new) => Err(MatchErr::TypeMismatch(new, old)),
  }
}
