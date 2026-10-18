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

def bit(b: Bool) -> String:
  match b:
    case True{}:
      "1"
    case False{}:
      "0"

def predicates(+a: Nat, +b: Nat) -> String:
  bit(Nat.is_eq(a, b)) ++ bit(Nat.is_ne(a, b)) ++ bit(Nat.is_le(a, b)) ++
  bit(Nat.is_gt(a, b)) ++ bit(Nat.is_ge(a, b))

def compared_text(p: String & String, c: Cmp) -> String:
  (s, t) = p
  codes(s) ++ "|" ++ codes(t) ++ ":" ++ order(c)

def compared(p: (String & String) & Cmp) -> String:
  (s, c) = p
  compared_text(s, c)

def keyed(p: String & Bool) -> String:
  (s, b) = p
  codes(s) ++ "|" ++ bit(b)

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
    IO.print(Nat.show(Nat.pow(0n, 0n)) ++ ":" ++ Nat.show(Nat.pow(Nat.add(Nat.mul(4294967295n, 65536n), 65535n), 1n)))
    IO.print(Nat.show(Nat.pow(3n, 17n)))
    IO.print(predicates(17n, 17n) ++ ":" ++ predicates(17n, 18n) ++ ":" ++ predicates(18n, 17n))
    IO.print(compared(String.cmp(s, s)))
    IO.print(compared(String.cmp(SCon{Chr{4294967295}, SNil{}}, SCon{Chr{0}, SNil{}})))
    IO.print(keyed(Map.bit(s, 100n)))
    IO.print(bit(Pair.snd(String, Bool, Map.bit(s, 0n))) ++ ":" ++
      bit(Pair.snd(String, Bool, Map.bit(s, 1n))) ++ ":" ++
      bit(Pair.snd(String, Bool, Map.bit(s, 50n))) ++ ":" ++
      bit(Pair.snd(String, Bool, Map.bit(s, 82n))) ++ ":" ++
      bit(Pair.snd(String, Bool, Map.bit(s, 165n))) ++ ":" ++
      bit(Pair.snd(String, Bool, Map.bit(s, n))))
    IO.print(Nat.show(Map.msb.u(0n, 4294967295)) ++ ":" ++
      Nat.show(Map.msb.u(1n, 4294967295)) ++ ":" ++
      Nat.show(Map.msb.u(32n, 0)) ++ ":" ++ Nat.show(Map.msb.u(32n, 1)) ++ ":" ++
      Nat.show(Map.msb.u(32n, 2147483648)) ++ ":" ++ Nat.show(Map.msb.u(n, 4294967295)))

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
#|1:281474976710655
#|129140163
#|10101:01100:01011
#|0:55296:65536:4294967295:97:|0:55296:65536:4294967295:97::EQ
#|4294967295:|0::GT
#|0:55296:65536:4294967295:97:|1
#|1:0:1:1:0:0
#|0:1:0:1:32:32
