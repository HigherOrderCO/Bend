# a bang over two value-indexed families that hold each other: the
# closure sits in an instance of the outer family nested in the inner one,
# and the walk still opens it
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

type A<-t: Tag> is Type:
  A{value: Pick(t), b: B<Num{}>}

type B<-t: Tag> is Type:
  BEnd{}
  B{value: Pick(t), a: A<Fun{}>}

def use(a: A<Num{}>) -> U32:
  match a:
    case A{x, b}:
      match b:
        case BEnd{}:
          x
        case B{y, a2}:
          match a2:
            case A{f, b2}:
              f(x)

def make() -> A<Num{}>:
  A{41, B{0, A{x => (x + 1 : U32), BEnd{}}}}

def main() -> IO(Unit):
  IO.print(U32.show(use!(make())))

#|42
