# a function in a field whose type is a parameter (a pair's, Some's): the
# call keep(v) passes v to --verdict eta-long, so the equation's bare
# (v, False{}) must go at its goal too, or its v stays bare, and the
# kernel, which converts without eta, cannot meet λx => v(x) with v
import Base

def keep(v: U32 -> U32) -> (U32 -> U32) & Bool:
  (v, False{})

def some(~T: Type, v: T) -> Maybe<&1, T>:
  Some{v}

def pair(v: U32 -> U32) -> {keep(v) == (v, False{}) : (U32 -> U32) & Bool}:
  {==}

def wrap(v: U32 -> U32) -> {some(~(U32 -> U32), v) == Some{v} : Maybe<&1, U32 -> U32>}:
  {==}

#|ALL PROOFS CHECK
#|Use --verdict for mathematical validity.
