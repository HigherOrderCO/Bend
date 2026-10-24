# A JS I32 is a signed number, not its bits: the word a match opens from it
# rebuilds a U32, an F32 or an I32 with the same bits, a closure that captures
# that word keeps the sign, and a table of I32 results answers signed numbers
# on JS and their bits in C.
import Base

def to_u(a: I32) -> U32:
  match a:
    case I32{w}:
      U32{w}

def to_i(a: U32) -> I32:
  match a:
    case U32{w}:
      I32{w}

def to_f(a: I32) -> F32:
  match a:
    case I32{w}:
      F32{w}

def low(a: I32) -> Bool:
  match a:
    case I32{WCon{h, t}}:
      h

def app(f: I32 -> I32) -> I32:
  f(1i)

def cap(a: I32) -> I32:
  match a:
    case I32{w}:
      app(x => I32.add(x, I32{w}))

def tab(n: U32) -> I32:
  match n:
    case 0:
      I32.neg(1i)
    case 1:
      I32.neg(2i)
    case 2:
      5i
    case 3:
      2147483647i
    case _:
      0i

def tabs(xs: List<U32>) -> List<I32>:
  match xs:
    case Nil{}:
      []
    case Con{x, t}:
      Con{tab(x), tabs(t)}

def main() -> List<U32> & List<I32> & List<Bool> & List<I32> & F32:
  ([to_u(I32.neg(5i)), to_u(7i)],
    [to_i(4294967291), to_i(7), cap(I32.neg(9i))],
    [low(I32.neg(1i)), low(I32.neg(2i))],
    tabs([0, 1, 2, 3, 9]),
    to_f(I32.neg(1082130432i)))

#|([4294967291, 7], [-5i, 7i, -8i], [True{}, False{}], [-1i, -2i, 5i, 2147483647i, 0i], -1.0)
