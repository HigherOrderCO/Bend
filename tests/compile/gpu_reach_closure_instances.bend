# Separate value-indexed Box instances must both participate in closure discovery.
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

type Box<-t: Tag> is Type:
  Box{value: Pick(t)}

type Duo is Type:
  Duo{plain: Box<Num{}>, callable: Box<Fun{}>}

def use(d: Duo) -> U32:
  match d:
    case Duo{plain, callable}:
      match plain:
        case Box{x}:
          match callable:
            case Box{f}:
              f(x)

def make() -> Duo:
  Duo{Box{41}, Box{x => (x + 1 : U32)}}

def main() -> IO(Unit):
  IO.print(U32.show(use!(make())))

#|42
