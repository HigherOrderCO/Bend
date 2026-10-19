# descent against a literal pattern is linear in the literal: 65n at
# the 64n column steps one Succ per level, and a field that fails to fit
# its own column's field is not searched again below it. the naive
# search retried every suffix and took 2^n steps (hung at 32n).
import Base

law down:
  for n: Nat
  Nat

def down(n):
  match n:
    case 64n:
      down(65n)
    case _:
      n

law main:
  Nat

def main(): down(3n)

#|SOME PROOFS FAIL
#|Error:
#|- message  : down's 1st argument, 65n, is not smaller than n
#|Location: down
#|13 |     case 64n:
#|14>|       down(65n)
#|   |       ^^^^
#|15 |     case _:
#|Note: pass a part of n, or mark the def @unsafe to skip the termination check.
#|exit 1
