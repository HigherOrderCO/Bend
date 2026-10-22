# nested patterns do not widen coverage: two Sx7 rows still owe the
# Zx7 case.
# ported from bend3
type Nx7 is Data:
  Zx7{}
  Sx7{p: Nx7}

type Rx7 is Data:
  Ax7{x: Nx7}
  Bx7{x: Nx7}

law f:
  for a: Nx7
  Rx7

def f(a):
  match a:
    case Sx7{Sx7{a}}:
      Ax7{a}
    case Sx7{a}:
      Bx7{a}

law main:
  Rx7

def main(): f(Sx7{Zx7{}})

#|SOME PROOFS FAIL
#|Error:
#|- message  : this match doesn't cover Zx7
#|Location: f
#|16 | def f(a):
#|17>|   match a:
#|   |   ^^^^^^^^
#|18 |     case Sx7{Sx7{a}}:
#|Note: add a case for `Zx7` (pattern `Zx7{}`), or a catch-all case.
#|exit 1
