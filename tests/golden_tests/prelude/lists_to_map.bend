# Takes two lists and uses one as keys and the other as values, returning a map
def test(m: Map(T), xs: List(u24), ys: List(T)) -> Map(T):
  match xs:
    case List/Nil:
      return Map/Leaf
    case List/Cons:
      match ys:
        case List/Nil:
          return Map/Leaf
        case List/Cons:
          return test(Map/set(m, xs.head, ys.head), xs.tail, ys.tail)

def main() -> _:
  return test(Map/Leaf, List/Cons(1, List/Nil), List/Cons(2, List/Nil))
