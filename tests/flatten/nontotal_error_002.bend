# NotTotal names the missing constructor of a partial match.
# ported from bend3
type Ne2 is Data:
  Ze2{}
  Se2{p: Ne2}

type Re2 is Data:
  Ae2{x: Ne2}
  Be2{x: Ne2}

law f:
  for a: Ne2
  Re2

def f(a):
  match a:
    case Se2{a}:
      Ae2{a}

law main:
  Re2

def main(): f(Se2{Ze2{}})

#|SOME PROOFS FAIL
#|Error:
#|- message  : this match doesn't cover Ze2
#|Location: f
#|15 | def f(a):
#|16>|   match a:
#|   |   ^^^^^^^^
#|17 |     case Se2{a}:
#|Note: add a case for `Ze2` (pattern `Ze2{}`), or a catch-all case.
#|exit 1
