# Concrete function elements have a uniform boxed layout. Captured closures
# survive storage, splitting and recursive consumption in Array cells.
import Base

def mkA(n: U32) -> Array<(U32 -> U32)>:
  ALeaf{x => (x + n : U32)}

def sumA(a: Array<(U32 -> U32)>) -> U32:
  match a:
    case ALeaf{f}: f(1)
    case ANode{xs, ys}: (sumA(xs) + sumA(ys) : U32)

def swapped(r: Array<(U32 -> U32)> & (U32 -> U32)) -> U32:
  (a, old) = r
  (old(7) + sumA(a) : U32)

def replace(a: Array<(U32 -> U32)>) -> U32:
  swapped(Array.swap(U32 -> U32, a, 0, x => (x + 1 : U32)))

def strings(s: String) -> Array<(String -> String)>:
  ALeaf{x => s ++ x}

def joinStrings(a: Array<(String -> String)>, +s: String) -> String:
  match a:
    case ALeaf{f}: f(s)
    case ANode{xs, ys}: joinStrings(xs, s) ++ joinStrings(ys, s)

# A generic domain: the element layout does not depend on T.
def applyAll(-T: Data, a: Array<(T -> U32)>, +x: T) -> U32:
  match a:
    case ALeaf{f}: f(x)
    case ANode{xs, ys}: (applyAll(T, xs, x) + applyAll(T, ys, x) : U32)

# Array.swap and Array.set under a generic domain.
def swapSet.got(-T: Data, r: Array<(T -> U32)> & (T -> U32), +x: T) -> U32:
  (b, old) = r
  (old(x) + applyAll(T, Array.set(T -> U32, b, 1, y => 30), x) : U32)

def swapSet(-T: Data, a: Array<(T -> U32)>, +x: T) -> U32:
  swapSet.got(T, Array.swap(T -> U32, a, 0, y => 7), x)

def bit(b: Bool) -> U32:
  match b:
    case True{}: 1
    case False{}: 0

# An erased argument: the cell still holds one boxed function.
def sumErased(a: Array<(@-m: Nat -> U32)>) -> U32:
  match a:
    case ALeaf{f}: f(3n)
    case ANode{xs, ys}: (sumErased(xs) + sumErased(ys) : U32)

# Array.new needs Data, so a function array is built leaf by leaf; both
# halves of each ANode have the same depth.
def build(+d: Nat, +i: U32) -> Array<(U32 -> U32)>:
  match d:
    case 0n:
      ALeaf{x => (x + i : U32)}
    case 1n+p:
      ANode{build(p, (i * 2 : U32)), build(p, (i * 2 + 1 : U32))}

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print(U32.show(sumA(ANode{mkA(41), mkA(6)})))
    IO.print(U32.show(replace(ANode{ALeaf{x => (x * 2 : U32)}, ALeaf{x => (x * 3 : U32)}})))
    IO.print(joinStrings(ANode{strings("a"), strings("b")}, "!"))
    IO.print(U32.show(applyAll(U32, ANode{ALeaf{x => (x + 1 : U32)}, ALeaf{x => (x * 2 : U32)}}, 5)))
    IO.print(U32.show(applyAll(Bool, ANode{ALeaf{b => bit(b)}, ALeaf{b => (bit(b) + 20 : U32)}}, True{})))
    IO.print(U32.show(swapSet(U32, ANode{ALeaf{x => (x + 1 : U32)}, ALeaf{x => (x * 2 : U32)}}, 5)))
    IO.print(U32.show(sumErased(ANode{ALeaf{m => 4}, ALeaf{m => 5}})))
    IO.print(U32.show(sumA(Array.set(U32 -> U32, build(2n, 0), 3, x => (x * 10 : U32)))))

#|49
#|19
#|a!b!
#|16
#|22
#|43
#|9
#|16
