import Base

def same(a: String, b: String) -> Bool:
  match a b:
    case SNil{} SNil{}:
      True{}
    case SCon{Chr{x}, xs} SCon{Chr{y}, ys}:
      Bool.and(U32.is_eq(x, y), same(xs, ys))
    case _ _:
      False{}

def check_repeat(+s: String) -> Bool:
  Bool.and(same(String.repeat(s, 3n), s ++ s ++ s),
    Bool.and(same(String.repeat(s, 0n), ""),
      same(String.repeat("", 256n), "")))

def main() -> Bool:
  check_repeat(SCon{Chr{4294901773}, SCon{Chr{65536}, SNil{}}})

#|True{}
