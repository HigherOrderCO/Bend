# a match missing a constructor's case is refused with the missing tag.
# ported from bend3
type Nx6 is Data:
  Zx6{}
  Sx6{p: Nx6}

type Rx6 is Data:
  Ax6{x: Nx6}

law f:
  for a: Nx6
  Rx6

def f(a):
  match a:
    case Sx6{a}:
      Ax6{a}

law main:
  Rx6

def main(): f(Sx6{Zx6{}})

#|SOME PROOFS FAIL
#|Error:
#|- message  : this match doesn't cover Zx6
#|Location: f
#|14 | def f(a):
#|15>|   match a:
#|   |   ^^^^^^^^
#|16 |     case Sx6{a}:
#|Note: add a case for `Zx6` (pattern `Zx6{}`), or a catch-all case.
#|exit 1
