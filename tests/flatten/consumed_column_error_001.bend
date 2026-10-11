# consumed means gone: after `match a` opens a's column, an inner
# `match a` in the Sw3 arm has no value left -- the scrutinee is
# already a constructor there and the match is refused.
# ported from bend3: the rebind let drops (bend lets cannot precede a
# match), the refusal is the undestructed-scrutinee page
type Nw3 is Data:
  Zw3{}
  Sw3{p: Nw3}

law f:
  for a: Nw3
  Nw3

def f(a):
  match a:
    case Zw3{}:
      Zw3{}
    case Sw3{p}:
      match a:
        case Zw3{}:
          Zw3{}
        case Sw3{q}:
          q

law main:
  Nw3

def main(): f(Zw3{})

#|SOME PROOFS FAIL
#|Error:
#|- message  : 'a' can't be matched here (match it in the same match as the pattern that introduced it)
#|Location:
#|18 |     case Sw3{p}:
#|19>|       match a:
#|   |       ^^^^^^^^
#|20 |         case Zw3{}:
#|exit 1
