# a hot constructor built at a family stuck on an open index: sharing the
# list heats Tuple, so the Tuple inside build seals its P field; P must
# be hot too, or total takes the sealed node raw and reads its cell.
# the family hides P from the bound types; the bind keeps the index open
# build only reads words, so it borrows; give owns, so main shares s
import Base

type P is Data:
  P{a: U32, b: U32}

def Q() -> Data:
  Sigma<&2,&2,U32,(x => U32)>

def Pw(xs: List<&2,Q()>) -> Data:
  match xs:
    case Nil{}: Unit
    case Con{h,t}: Sigma<&2,&2,P,(p => Unit)>

def build(xs: List<&2,Q()>) -> Pw(xs):
  match xs:
    case Nil{}: Unit{}
    case Con{Tuple{n,m},t}: Tuple{P{n,1},Unit{}}

def give(xs: List<&2,Q()>) -> List<&2,Q()>:
  xs

def total(xs: List<&2,Q()>, ps: Pw(xs)) -> U32:
  match xs:
    case Nil{}: 0
    case Con{h,t}:
      match ps:
        case Tuple{P{a,b},u}: a

def main() -> IO(Unit):
  do IO<Unit>:
    +s : List<&2,Q()> <- IO.pure(List<&2,Q()>, [Tuple{5,3}])
    IO.print(U32.show(total(s,build(give(s)))))

#|5
