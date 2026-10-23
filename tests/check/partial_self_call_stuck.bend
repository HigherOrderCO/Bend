# a partial self-call whose supplied columns stay EQ has not descended:
# the closure could be applied to anything, so it is refused as any
# non-decreasing call is.
import Base

law part:
  for a: Nat
  for b: Nat
  Nat

def part(a, b):
  match a:
    case 0n:
      b
    case 1n+p:
      g = part(a)
      g(p)

law main:
  Nat

def main(): part(2n, 1n)

#|SOME PROOFS FAIL
#|Error:
#|- message  : part's 1st argument, 1n+p, is not smaller than a
#|Context:
#|- p : Nat
#|- b : Nat
#|Location: part
#|15 |     case 1n+p:
#|16>|       g = part(a)
#|   |           ^^^^
#|17 |       g(p)
#|Note: pass a part of a, or mark the def @unsafe to skip the termination check.
#|exit 1
