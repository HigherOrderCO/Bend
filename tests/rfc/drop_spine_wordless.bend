# drop order: a field can hold the node's own datatype with no box for it
# (here a phantom Ph<PL>, as Nat's Succ holds a Nat word). Such a node has
# no spine slot, and its CID_T entry stays 255: a -1 there is a narrowing
# error in the Metal build of a program with a `!` call.
import Base

type Ph<-A: Type> is Data:
  Ph{}

type PL is Data:
  PNil{}
  PCon{tag: Ph<PL>, x: U32, y: List<&2, U32>}

def mk(+n: Nat) -> PL:
  match n:
    case 0n:
      PNil{}
    case 1n+m:
      PCon{Ph{}, U32.from_nat(n), [1]}

def get(p: PL) -> U32:
  match p:
    case PNil{}:
      0
    case PCon{t, x, y}:
      x

def main() -> IO(Unit):
  IO.print(U32.show(get!(mk(5n))))

#|5
