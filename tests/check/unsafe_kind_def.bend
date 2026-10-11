# An @unsafe reference in a def body reaches a datatype through its kind
# and then reaches a def that uses the datatype.
import Base

def qsize(j: Array<Nat>) -> Quant:
  match j:
    case ALeaf{value}: &1
    case ANode{xs, ys}: &1

def qv() -> Quant:
  qsize(Array.join(Nat, ALeaf{0n}, ALeaf{0n}))

type D is Kind(qv()):
  Mk{}

def use(d: D) -> Nat:
  match d:
    case Mk{}: 0n

#|SOME PROOFS FAIL
#|Error: 3 defs rely on unsafe or foreign code:
#|- qv
#|- D
#|- use
#|exit 1
