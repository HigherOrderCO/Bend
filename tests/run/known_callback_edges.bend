# literal callbacks at the edges of specialization: a generic callee
# whose recursion passes a lambda with an erased argument, callbacks
# that trade places, a continuation cycle (String.word's shape), a
# callback a local lambda captures or a closure returns, a polymorphic
# callback, and a lambda applied in place
import Base

def ap(-R: Type, x: U32, f: U32 -> R) -> R:
  f(x)

def id(-A: Type, x: A) -> A:
  x

def go(-A: Type, n: Nat, x: A, f: A -> A) -> A:
  match n:
    case 0n:
      f(x)
    case 1n+p:
      go(A, p, f(x), y => id(A, y))

def sw(n: Nat, f: U32 -> U32, g: U32 -> U32) -> U32:
  match n:
    case 0n:
      f(g(1))
    case 1n+p:
      sw(p, g, f)

def word.go(
  -R: Type, sp: Bool, c: Char, t: String, k: String -> String -> R,
  rest: (String -> String -> R) -> R
) -> R:
  match sp:
    case True{}:
      k(SNil{}, t)
    case False{}:
      rest(w => r => k(SCon{c, w}, r))

def word(-R: Type, +s: String, k: String -> String -> R) -> R:
  match s:
    case SNil{}:
      k(SNil{}, SNil{})
    case SCon{Chr{c}, t}:
      word.go(R, U32.is_eq(c, 32), Chr{c}, t, k, k2 => word(R, t, k2))

def inner(x: U32, f: U32 -> U32) -> U32:
  g : U32 -> U32 = y => f(U32.mul(y, 2))
  g(x)

def mk(f: U32 -> U32) -> U32 -> U32:
  x => f(x)

def sel(x: U32, k: U32 -> U32) -> U32:
  match x:
    case 0:
      k(100)
    case 1:
      k(200)
    case _:
      k(x)

def poly(+x: U32, f: @-A:Type -> List<A> -> U32) -> U32:
  f(U32, [x, x])

def len(-A: Type, xs: List<A>) -> U32:
  match xs:
    case []:
      0
    case h <> t:
      (1 + len(A, t) : U32)

def three(+s: String) -> String:
  word(String, s, a => r1 =>
    word(String, r1, b => r2 =>
      word(String, r2, c => r3 => c ++ "-" ++ b ++ "-" ++ a ++ "|" ++ r3)))

def run(+a: U32, +b: U32) ->
  U32 & U32 & U32 & U32 & U32 & U32 & U32 & U32:
  (go(U32, 3n, a, y => U32.add(y, 1)),
    sw(3n, y => U32.add(y, a), y => U32.mul(y, b)),
    sw(4n, y => U32.add(y, a), y => U32.mul(y, b)),
    inner(a, y => U32.add(y, b)), mk(y => U32.add(y, a))(b),
    (x => y => U32.sub(x, y))(a, b), U32.add(sel(0, y => U32.add(y, b)),
      sel(7, y => U32.add(y, b))), poly(b, A => ys => U32.add(len(A, ys), b)))

def main() ->
  String & U32 & U32 & U32 & U32 & U32 & U32 & U32 & U32:
  (three("one two three four"), run(10, 3))

#|("three-two-one|four", 11, 33, 13, 23, 13, 7, 113, 5)
