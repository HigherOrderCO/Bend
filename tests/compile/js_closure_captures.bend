# A lifted JS closure takes what it reads from its def and from the
# closures around it: the outer closure carries k for the inner one, each
# turn of a tail loop hands its own p, and a let's value goes in once.
import Base

type Box is Type:
  H{f: U32 -> U32}

type Fn2 is Type:
  G{f: U32 -> Box}

type Fns is Type:
  FNil{}
  FCon{h: U32 -> U32, t: Fns}

def use(box: Box, x: U32) -> U32:
  match box:
    case H{f}: f(x)

def use2(g: Fn2, a: U32, b: U32) -> U32:
  match g:
    case G{f}: use(f(a), b)

def nest(k: U32) -> Fn2:
  G{a => H{b => U32.add(U32.mul(a, 10), U32.add(b, k))}}

def mk(+n: Nat, fs: Fns) -> Fns:
  match n:
    case 0n:
      fs
    case 1n+p:
      mk(p, FCon{x => U32.add(x, U32.add(U32.from_nat(p), 1)), fs})

def run(fs: Fns, acc: U32) -> U32:
  match fs:
    case FNil{}:
      acc
    case FCon{h, t}:
      run(t, U32.add(U32.mul(acc, 10), h(0)))

def held(+k: U32) -> Box:
  m = U32.mul(k, k)
  H{x => U32.add(x, m)}

def main() -> IO(Unit):
  do IO<Unit>:
    t : Nat <- IO.now()
    IO.print(U32.show(use2(nest(U32.add(3, U32.from_nat(Nat.mod(t, 1n)))), 4, 5)))
    u : Nat <- IO.now()
    IO.print(U32.show(run(mk(Nat.add(3n, Nat.mod(u, 1n)), FNil{}), 0)))
    v : Nat <- IO.now()
    IO.print(U32.show(use(held(U32.add(5, U32.from_nat(Nat.mod(v, 1n)))), 1)))

#|48
#|123
#|26
