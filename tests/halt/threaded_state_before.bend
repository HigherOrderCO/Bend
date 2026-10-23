# bend3's found-column search accepted a growing accumulator BEFORE
# the decreasing column; bend's left-to-right law refuses a non-EQ
# column ahead of the descent (halt_threaded_state_after pins the
# accepted order)
import Base

law go:
  for acc: U32
  for n: Nat
  U32

def go(acc, n):
  match n:
    case Zero{}:
      acc
    case Succ{p}:
      go((acc + 1 : U32), p)

law main:
  Nat

def main():
  U32.to_nat(go(10, 3n))

#|SOME PROOFS FAIL
#|Error:
#|- message  : go's 1st argument, U32.add(acc, 1), is not smaller than acc
#|Context:
#|- acc : U32
#|- p   : Nat
#|Location: go
#|16 |     case Succ{p}:
#|17>|       go((acc + 1 : U32), p)
#|   |       ^^
#|18 |
#|Note: pass a part of acc, or mark the def @unsafe to skip the termination check.
#|exit 1
