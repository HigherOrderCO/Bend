# a slow closed call inside a type converts with its copy when another
# part of the type needs one unfold: Same(Unit) is not rigidly Unit, and
# Heavy() normalizes through about 2^26 steps of slow
import Base

def slow(+n: Nat) -> Nat:
  match n:
    case 0n: 0n
    case 1n+p: Nat.add(slow(p), slow(p))

def Heavy() -> Data:
  {slow(26n) == 0n : Nat}

def Same(x: Data) -> Data:
  x

def Both(x: Data, y: Data) -> Data:
  Sigma<&2, &2, x, _ => y>

def cast(w: Both(Heavy(), Same(Unit))) -> Both(Heavy(), Unit):
  w

#|ALL PROOFS CHECK
#|Use --verdict for mathematical validity.
