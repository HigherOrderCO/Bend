# the descent verdict inside one constructor column is not
# lexicographic: fields compare left to right and every non-EQ result
# overwrites the verdict, so a later growing field overrules an earlier
# shrinking one (LT then GT is GT) and the self-call refuses. pins the
# law against a lax first-non-EQ respelling, which accepts this program
# whole.
import Base

type PairN is Data:
  MkP{fst: Nat, snd: Nat}

law wob:
  for p: PairN
  Nat

def wob(p):
  match p:
    case MkP{Zero{}, b}:
      b
    case MkP{Succ{a2}, b}:
      wob(MkP{a2, Succ{b}})

law main:
  Nat

def main(): wob(MkP{2n, 0n})

#|SOME PROOFS FAIL
#|Error:
#|- message  : wob's 1st argument, MkP{a2, 1n+b}, is not smaller than p
#|Context:
#|- a2 : Nat
#|- b  : Nat
#|Location: wob
#|20 |     case MkP{Succ{a2}, b}:
#|21>|       wob(MkP{a2, Succ{b}})
#|   |       ^^^
#|22 |
#|Note: pass a part of p, or mark the def @unsafe to skip the termination check.
#|exit 1
