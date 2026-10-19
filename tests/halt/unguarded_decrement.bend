# an unguarded decrement is NOT smaller: without a match the argument
# may already be zero (Nat.sub truncates), so f(Nat.sub(d, 1n)) shrinks
# no column and the self-call is refused.
# ported from bend3: the U32 wrap-around argument becomes Nat.sub
import Base

law f:
  for d: Nat
  Nat

def f(d):
  f(Nat.sub(d, 1n))

law main:
  Nat

def main(): 0n

#|SOME PROOFS FAIL
#|Error:
#|- message  : f's 1st argument, Nat.sub(d, 1n), is not smaller than d
#|Context:
#|- d : Nat
#|Location: f
#|11 | def f(d):
#|12>|   f(Nat.sub(d, 1n))
#|   |   ^
#|13 |
#|Note: pass a part of d, or mark the def @unsafe to skip the termination check.
#|exit 1
