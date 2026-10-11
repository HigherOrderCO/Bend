# regression witness: ty_wnf keyed a type by term_lower at depth 0, so
# main's closed pick(X => b => X) and gen's pick(X => b => Y) at Y := X,
# whose X is a free level-0 variable, shared one key. gen read U32 for
# its z, a word, and kept no count for the Chain it shares: sum(a) freed
# it, build reused its cells and sum(b) read them (846393204, not 100).
import Base

def pick(g: Data -> Data -> Data) -> Data:
  g(U32)(String)

def use(Y: Data, y: pick(X => b => Y)) -> pick(X => b => Y):
  y

def gen(X: Data, y: X) -> X & X:
  +z = use(X, y)
  (z, z)

type Chain is Data:
  CNil{}
  CCon{h: U32, t: Chain}

def sum(xs: Chain) -> U32:
  match xs:
    case CNil{}:
      0
    case CCon{h, t}:
      (h + sum(t) : U32)

def build(n: Nat, +v: U32) -> Chain:
  match n:
    case 0n:
      CNil{}
    case 1n+p:
      CCon{v, build(p, v)}

def show(p: Chain & Chain, n: U32) -> IO(Unit):
  (a, b) = p
  x = sum(a)
  y = sum(build(100n, 1000))
  w = sum(b)
  do IO<Unit>:
    IO.print(U32.show(x))
    IO.print(U32.show(y))
    IO.print(U32.show(w))
    IO.print(U32.show(n))

def main() -> IO(Unit):
  n = {7 : pick(X => b => X)}
  show(gen(Chain, build(100n, 1)), n)

#|100
#|100000
#|100
#|7
