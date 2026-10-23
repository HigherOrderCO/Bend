# permanent pin: a two-param def whose 1n arm re-calls at the SAME
# literal spending the OTHER param -- the first column stays EQ, and
# Nat.sub(y, 1n) is no subterm of y, so the self-call is refused. An
# old bend3 depth-slot slip briefly accepted this shape; it must stay
# refused.
# ported from bend3: U32 literals become Nat literals, y - 1 becomes
# Nat.sub
import Base

law f:
  for x: Nat
  for y: Nat
  Nat

def f(x, y):
  match x:
    case 0n:
      0n
    case 1n:
      f(1n, Nat.sub(y, 1n))
    case p:
      0n

law main:
  Nat

def main(): f(0n, 0n)

#|SOME PROOFS FAIL
#|Error:
#|- message  : f's 1st argument, 1n, is not smaller than x
#|Context:
#|- y : Nat
#|Location: f
#|19 |     case 1n:
#|20>|       f(1n, Nat.sub(y, 1n))
#|   |       ^
#|21 |     case p:
#|Note: pass a part of x, or mark the def @unsafe to skip the termination check.
#|exit 1
