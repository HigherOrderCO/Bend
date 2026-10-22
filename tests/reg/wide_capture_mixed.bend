# Ten two-word pairs and one 240-word record exceed WIDE.
# Only the record needs a box. Native checks inspect the closure widths.
# A parameter box must stay boxed when the next capture fits below WIDE.
# The first closure has 20 pair words, one box and one argument.
# The second has one box, one sum and one argument.
import Base

type Duo is Data:
  Duo{a: U32, b: U32}

type Wide is Data:
  Wide{bits: Word(239n), text: String}

def fill(n: Nat, +bit: Bool) -> Word(n):
  match n:
    case 0n: WNil{}
    case 1n+p: WCon{bit, fill(p, bit)}

def pair(p: Duo) -> U32:
  match p:
    case Duo{a, b}: U32.add(a, b)

def truth(b: Bool) -> U32:
  match b:
    case True{}: 1
    case False{}: 999

def wide(w: Wide) -> U32:
  match w:
    case Wide{bits, text}:
      truth(Bool.and(Cmp.is_eq(Word.cmp(239n, bits, fill(239n, True{}))), String.eq(text, "wide")))

def sum(a: Duo, b: Duo, c: Duo, d: Duo, e: Duo, f: Duo, g: Duo, h: Duo, i: Duo, j: Duo, w: Wide) -> U32:
  U32.add(wide(w), U32.add(U32.add(U32.add(pair(a), pair(b)), U32.add(pair(c), pair(d))),
    U32.add(U32.add(pair(e), pair(f)), U32.add(U32.add(pair(g), pair(h)), U32.add(pair(i), pair(j))))))

def inspect(f: U32 -> U32, arity: U32) -> IO(U32):
  import "./wide_capture_mixed.c"
  import "./wide_capture_mixed.js"

def closure(+a: Duo, +b: Duo, +c: Duo, +d: Duo, +e: Duo, +f: Duo, +g: Duo, +h: Duo, +i: Duo, +j: Duo, u: U32, text: String) -> IO(U32):
  +w : Wide = Wide{fill(239n, U32.is_eq(u, 1)), text}
  fun : U32 -> U32 = x => U32.add(x, sum(a, b, c, d, e, f, g, h, i, j, w))
  do IO<U32>:
    got : U32 <- inspect(fun, 22)
    IO.pure(U32, U32.add(got, sum(a, b, c, d, e, f, g, h, i, j, w)))

def retained(a: Duo, b: Duo, c: Duo, d: Duo, e: Duo, f: Duo, g: Duo, h: Duo, i: Duo, j: Duo, +w: Wide) -> IO(U32):
  got : U32 = U32.add(U32.add(U32.add(pair(a), pair(b)), U32.add(pair(c), pair(d))),
    U32.add(U32.add(pair(e), pair(f)), U32.add(U32.add(pair(g), pair(h)), U32.add(pair(i), pair(j)))))
  fun : U32 -> U32 = x => U32.add(x, U32.add(got, U32.add(wide(w), wide(w))))
  inspect(fun, 3)

def step(x: U32) -> U32:
  U32.add(x, 1)

def nonflat(+x: U32) -> U32:
  a b = step(x) step(x)
  U32.add(a, b)

def call(+a: Duo, +b: Duo, +c: Duo, +d: Duo, +e: Duo, +f: Duo, +g: Duo, +h: Duo, +i: Duo, +j: Duo, u: U32, text: String) -> U32:
  +w : Wide = Wide{fill(239n, U32.is_eq(u, 1)), text}
  got = nonflat(10)
  U32.add(got, U32.add(sum(a, b, c, d, e, f, g, h, i, j, w), sum(a, b, c, d, e, f, g, h, i, j, w)))

def fork(+a: Duo, +b: Duo, +c: Duo, +d: Duo, +e: Duo, +f: Duo, +g: Duo, +h: Duo, +i: Duo, +j: Duo, u: U32, text: String) -> U32:
  +w : Wide = Wide{fill(239n, U32.is_eq(u, 1)), text}
  x y = step(10) step(10)
  U32.add(U32.add(x, y), U32.add(sum(a, b, c, d, e, f, g, h, i, j, w), sum(a, b, c, d, e, f, g, h, i, j, w)))

def main() -> IO(Unit):
  do IO<Unit>:
    args : List<String> <- IO.args()
    +n : Nat = List.length(&1, String, args)
    +u : U32 = U32.from_nat(n)
    +p : Duo = Duo{u, u}
    +s : String = String.repeat("wide", n)
    got : U32 <- closure(p, p, p, p, p, p, p, p, p, p, u, s)
    a : Unit <- IO.print(U32.show(got))
    b : Unit <- IO.print(U32.show(call(p, p, p, p, p, p, p, p, p, p, u, s)))
    c : Unit <- IO.print(U32.show(fork(p, p, p, p, p, p, p, p, p, p, u, s)))
    held : U32 <- retained(p, p, p, p, p, p, p, p, p, p, Wide{fill(239n, U32.is_eq(u, 1)), s})
    d : Unit <- IO.print(U32.show(held))
    IO.print(U32.show(truth(String.eq(s, "wide"))))

#|49
#|64
#|64
#|29
#|1
