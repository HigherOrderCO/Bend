# a bang over two instances of a value-indexed family, each under ten
# type arguments: the first holds a word, the second a closure. The walk
# opens each instance by its index however deep it sits, so it finds the
# closure: the lambda minted on the host is on the device
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

type Bx<A: Type> is Type:
  Bx{v: A}

type Box<-t: Tag> is Type:
  Box{value: Pick(t)}

type Duo is Type:
  Duo{plain: Bx<Bx<Bx<Bx<Bx<Bx<Bx<Bx<Bx<Bx<Box<Num{}>>>>>>>>>>>,
    callable: Bx<Bx<Bx<Bx<Bx<Bx<Bx<Bx<Bx<Bx<Box<Fun{}>>>>>>>>>>>}

def word(b: Bx<Bx<Bx<Bx<Bx<Bx<Bx<Bx<Bx<Bx<Box<Num{}>>>>>>>>>>>) -> U32:
  match b:
    case Bx{b0}:
      match b0:
        case Bx{b1}:
          match b1:
            case Bx{b2}:
              match b2:
                case Bx{b3}:
                  match b3:
                    case Bx{b4}:
                      match b4:
                        case Bx{b5}:
                          match b5:
                            case Bx{b6}:
                              match b6:
                                case Bx{b7}:
                                  match b7:
                                    case Bx{b8}:
                                      match b8:
                                        case Bx{b9}:
                                          match b9:
                                            case Box{x}:
                                              x

def call(b: Bx<Bx<Bx<Bx<Bx<Bx<Bx<Bx<Bx<Bx<Box<Fun{}>>>>>>>>>>>) -> U32 -> U32:
  match b:
    case Bx{b0}:
      match b0:
        case Bx{b1}:
          match b1:
            case Bx{b2}:
              match b2:
                case Bx{b3}:
                  match b3:
                    case Bx{b4}:
                      match b4:
                        case Bx{b5}:
                          match b5:
                            case Bx{b6}:
                              match b6:
                                case Bx{b7}:
                                  match b7:
                                    case Bx{b8}:
                                      match b8:
                                        case Bx{b9}:
                                          match b9:
                                            case Box{f}:
                                              f

def use(d: Duo) -> U32:
  match d:
    case Duo{plain, callable}:
      call(callable)(word(plain))

def main() -> IO(Unit):
  IO.print(U32.show(use!(Duo{Bx{Bx{Bx{Bx{Bx{Bx{Bx{Bx{Bx{Bx{Box{41}}}}}}}}}}},
    Bx{Bx{Bx{Bx{Bx{Bx{Bx{Bx{Bx{Bx{Box{x => (x + 1 : U32)}}}}}}}}}}}})))

#|42
