# a non-decreasing recursive call is rejected: descent needs some live
# argument to shrink (the rebuilt argument does not).
# ported from bend3
type Bx4 is Data:
  Tx4{}
  Fx4{}

law flip:
  for a: Bx4
  Bx4

def flip(a):
  match a:
    case Tx4{}:
      Fx4{}
    case Fx4{}:
      Tx4{}

law bad:
  for b: Bx4
  Bx4

def bad(b):
  match b:
    case Tx4{}:
      bad(flip(Tx4{}))
    case Fx4{}:
      Fx4{}

law main:
  Bx4

def main(): Fx4{}

#|SOME PROOFS FAIL
#|Error:
#|- message  : bad's 1st argument, flip(Tx4{}), is not smaller than b
#|Location: bad
#|25 |     case Tx4{}:
#|26>|       bad(flip(Tx4{}))
#|   |       ^^^
#|27 |     case Fx4{}:
#|Note: pass a part of b, or mark the def @unsafe to skip the termination check.
#|exit 1
