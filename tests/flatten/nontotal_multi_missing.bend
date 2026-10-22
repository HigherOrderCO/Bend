# error quality: a match missing several constructors names each one.
# ported from bend3
type Abc is Data:
  Aa1{}
  Bb1{}
  Cc1{}

law f:
  for x: Abc
  Abc

def f(x):
  match x:
    case Aa1{}:
      Aa1{}

law main:
  Abc

def main(): f(Aa1{})

#|SOME PROOFS FAIL
#|Error:
#|- message  : this match doesn't cover Bb1, Cc1
#|Location: f
#|12 | def f(x):
#|13>|   match x:
#|   |   ^^^^^^^^
#|14 |     case Aa1{}:
#|Note: add cases for `Bb1` (pattern `Bb1{}`), `Cc1` (pattern `Cc1{}`), or a catch-all case.
#|exit 1
