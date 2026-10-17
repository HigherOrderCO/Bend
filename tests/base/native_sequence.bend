import Base

def text() -> String:
  String.from_list([Chr{0}, Chr{55296}, Chr{65536}, Chr{4294967295}, Chr{97}])

def codes(s: String) -> String:
  match s:
    case SNil{}:
      ""
    case SCon{Chr{h}, t}:
      U32.show(h) ++ ":" ++ codes(t)

def got(m: Maybe<&2, Char>) -> String:
  match m:
    case None{}:
      "None"
    case Some{Chr{n}}:
      U32.show(n)

def order(c: Cmp) -> String:
  match c:
    case LT{}:
      "LT"
    case EQ{}:
      "EQ"
    case GT{}:
      "GT"

def power(f: @+a: U32 -> Nat -> U32, a: U32, n: Nat) -> U32:
  f(a, n)

def main() -> IO(Unit):
  +s = text()
  +n = Nat.double(U32.to_nat(2147483648))
  do IO<Unit>:
    IO.print(codes(s))
    IO.print(order(String.order(SCon{Chr{65536}, SNil{}}, SCon{Chr{57344}, SNil{}})))
    IO.print(order(String.order(SCon{Chr{4294967295}, SNil{}}, SCon{Chr{0}, SNil{}})))
    IO.print(order(String.order(s, s)))
    IO.print(got(String.get(s, 1n)) ++ ":" ++ got(String.get(s, 2n)) ++ ":" ++ got(String.get(s, 3n)))
    IO.print(got(String.get(s, 5n)) ++ ":" ++ got(String.get(s, n)))
    IO.print(U32.show(U32.pow(0, 0n)) ++ ":" ++ U32.show(U32.pow(4294967295, 2n)))
    IO.print(U32.show(power(U32.pow, 2, n)) ++ ":" ++ U32.show(U32.pow(3, 32n)))
    IO.print(Nat.show(U32.log2(0)) ++ ":" ++ Nat.show(U32.log2(1)) ++ ":" ++ Nat.show(U32.log2(2)))
    IO.print(Nat.show(U32.log2(2147483647)) ++ ":" ++ Nat.show(U32.log2(2147483648)) ++ ":" ++ Nat.show(U32.log2(4294967295)))

#|0:55296:65536:4294967295:97:
#|GT
#|GT
#|EQ
#|55296:65536:4294967295
#|None:None
#|1:1
#|0:3793632897
#|0:0:1
#|30:31:31
