# multi-scrutinee match: a non-exhaustive case set still errors
# NotTotal.
# ported from bend3
type Bity0 is Data:
  Ob{}
  Ib{}

law mix:
  for a: Bity0
  for b: Bity0
  Bity0

def mix(a, b):
  match a, b:
    case Ob{}, Ob{}:
      Ob{}
    case Ib{}, Ib{}:
      Ib{}

law main:
  Bity0

def main(): mix(Ob{}, Ob{})

#|SOME PROOFS FAIL
#|Error:
#|- message  : this match doesn't cover Ib
#|Location: mix
#|13 | def mix(a, b):
#|14>|   match a, b:
#|   |   ^^^^^^^^^^^
#|15 |     case Ob{}, Ob{}:
#|Note: add `case Ib{}:` (or `case _:`).
#|exit 1
