import Base

type V.Nil is Data:
  VNil{}

type V.Con<-p: Nat> is Data:
  VCon{head: Bool, tail: V(p)}

def V(n: Nat) -> Data:
  match n:
    case 0n:
      V.Nil
    case 1n+p:
      V.Con<p>

def drop(-n: Nat, w: V(n)) -> U32:
  match w:
    case rest:
      42

def mk(b: Bool) -> V(2n):
  VCon{b, VCon{True{}, VNil{}}}

def take(x: V(2n)) -> U32:
  match x:
    case VCon{head, tail}:
      drop!(1n, tail)

def main() -> U32:
  take(mk(True{}))

#|42
