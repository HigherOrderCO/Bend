# a bang over a chain whose growing index sits under twelve type
# arguments: the next link re-enters the chain with its index grown, so
# the walk opens it once with the index unknown and ends
import Base

type Bx<A: Type> is Type:
  Bx{v: A}

type Ch<-n: Nat> is Type:
  End{}
  Link{v: U32, next: Bx<Bx<Bx<Bx<Bx<Bx<Bx<Bx<Bx<Bx<Bx<Bx<Ch<(1n + n : Nat)>>>>>>>>>>>>>}

def use(c: Ch<0n>) -> U32:
  match c:
    case End{}:
      5
    case Link{x, next}:
      x

def main() -> IO(Unit):
  IO.print(U32.show(use!(End{})))

#|5
