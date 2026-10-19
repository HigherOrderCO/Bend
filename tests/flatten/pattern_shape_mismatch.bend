# mixed numeric/ctor columns: bend3 refused "mismatched pattern shapes
# (this column matches numbers)". In bend a literal pattern IS a ctor
# pattern (the U32 word chain), so the FZero row rides no word column,
# drops from every branch, and the mix surfaces as the 0-chain's own
# coverage hole
import Base

type Foo is Data:
  FZero{}

type R is Data:
  A{}
  B{}

law main:
  for a: U32
  R

def main(a):
  match a:
    case 0:
      A{}
    case FZero{}:
      B{}

#|SOME PROOFS FAIL
#|Error:
#|- message  : this match doesn't cover True
#|Location: main
#|19 | def main(a):
#|20>|   match a:
#|   |   ^^^^^^^^
#|21 |     case 0:
#|Note: add `case True{}:` (or `case _:`).
#|exit 1
