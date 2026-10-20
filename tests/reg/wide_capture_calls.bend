# A call and a fork save four 65-word values with shared Strings.
# The sequential continuations and parallel join must fit the segment limit.
import Base

type Bundle is Data:
  Bundle{word: Word(64n), text: String}

def fill(n: Nat, +bit: Bool) -> Word(n):
  match n:
    case 0n: WNil{}
    case 1n+p: WCon{bit, fill(p, bit)}

def truth(b: Bool) -> U32:
  match b:
    case False{}: 999
    case True{}: 1

def score(b: Bundle) -> U32:
  match b:
    case Bundle{w, text}:
      truth(Bool.and(Cmp.is_eq(Word.cmp(64n, w, fill(64n, True{}))),
        String.eq(text, "wide")))

def step(x: U32) -> U32:
  U32.add(x, 1)

def nonflat(+x: U32) -> U32:
  y z = step(x) step(x)
  U32.add(y, z)

def sum(a: Bundle, b: Bundle, c: Bundle, d: Bundle, x: U32) -> U32:
  U32.add(x, U32.add(U32.add(score(a), score(b)), U32.add(score(c), score(d))))

def call(+a: Bundle, +b: Bundle, +c: Bundle, +d: Bundle, x: U32) -> U32:
  y = nonflat(x)
  sum(a, b, c, d, sum(a, b, c, d, y))

def fork(+a: Bundle, +b: Bundle, +c: Bundle, +d: Bundle, +x: U32) -> U32:
  y z = step(x) step(x)
  sum(a, b, c, d, sum(a, b, c, d, U32.add(y, z)))

def main() -> IO(Unit):
  do IO<Unit>:
    args : List<String> <- IO.args()
    +n : Nat = List.length(&1, String, args)
    +w : Word(64n) = fill(64n, Cmp.is_eq(Nat.cmp(n, 1n)))
    +s : String = String.repeat("wide", n)
    u : Unit <- IO.print(U32.show(call(Bundle{w, s}, Bundle{w, s}, Bundle{w, s}, Bundle{w, s}, 10)))
    IO.print(U32.show(fork(Bundle{w, s}, Bundle{w, s}, Bundle{w, s}, Bundle{w, s}, 10)))

#|30
#|30
