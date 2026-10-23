# a claim of Data-kindedness for every T is a def like any other: its
# fill is live, so a circular fill is a live self-reference, must
# decrease (no live column shrinks: T is erased) and cannot
# manufacture Data for every T
import Base

law Forge:
  for -T: Type
  Data

def Forge(T):
  Forge(T)

law main:
  Nat

def main():
  0n

#|SOME PROOFS FAIL
#|Error:
#|- message  : Forge's 1st argument, T, is not smaller than T
#|Context:
#|- T : Type
#|Location: Forge
#|11 | def Forge(T):
#|12>|   Forge(T)
#|   |   ^^^^^
#|13 |
#|Note: pass a part of T, or mark the def @unsafe to skip the termination check.
#|exit 1
