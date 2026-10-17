# a bang over a chain whose index grows with each link: the walk opens
# the family once, with its index unknown, and stops at the next link;
# the value is the plain call's
import Base

type Ch<-n: Nat> is Type:
  End{}
  Link{v: U32, next: Ch<(1n + n : Nat)>}

def use(c: Ch<0n>) -> U32:
  match c:
    case End{}:
      0
    case Link{x, next}:
      x

def main() -> IO(Unit):
  IO.print(U32.show(use!(Link{5, End{}})))

#|5
