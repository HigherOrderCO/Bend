# no finite model exists: the safe corpus must report out of scope,
# not spend its timeout growing nested parameters and model values.
import Base

type Grow<-A: Type> is Type:
  GCons{x: A, rest: Grow<Pair(A, A)>}

def reflexive(~n: Grow<Nat>) -> {n == n : Grow<Nat>}:
  {==}

#|ALL PROOFS CHECK
#|Use --verdict for mathematical validity.
