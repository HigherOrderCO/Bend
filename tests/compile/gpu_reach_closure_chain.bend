# a bang over a chain whose later links are another instance of its own
# value-indexed family: the first link holds a word, the next a closure.
# The walk opens each instance, so the lambda minted on the host is on
# the device
import Base

type Tag is Data:
  Num{}
  Fun{}

def Pick(t: Tag) -> Type:
  match t:
    case Num{}:
      U32
    case Fun{}:
      U32 -> U32

type Chain<-t: Tag> is Type:
  End{}
  Link{value: Pick(t), next: Chain<Fun{}>}

def use(c: Chain<Num{}>) -> U32:
  match c:
    case End{}:
      0
    case Link{x, next}:
      match next:
        case End{}:
          x
        case Link{f, rest}:
          f(x)

def make() -> Chain<Num{}>:
  Link{41, Link{x => (x + 1 : U32), End{}}}

def main() -> IO(Unit):
  IO.print(U32.show(use!(make())))

#|42
