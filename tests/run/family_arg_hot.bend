# a hot constructor built at a family stuck on an open index, whose arm
# is a type argument: sharing a Carrier heats it, so the Carrier inside
# pack seals its F(n, RT) field; RT must be hot too, or use takes the
# sealed node as a raw RT. The arm's type comes from the argument, not
# from the family's body, so the facts walk the arguments at every
# instantiation. The index comes from a fold the checker cannot see.
# first only reads words, so it borrows; give owns, so heat shares c.
import Base

type RT is Data:
  RT{a: U32, b: U32, c: U32}

law F:
  for n: Nat
  for A: Data
  Data

def F(n, A):
  match n:
    case 0n:
      U32
    case 1n+p:
      A

type Carrier<-n: Nat> is Data:
  Carrier{item: F(n, RT), tail: Maybe<&2, Carrier<n>>}

def hold(n: Nat) -> Nat:
  n

def opaque_one() -> Nat:
  a b = hold(0n) hold(0n)
  1n+a

def make(+n: Nat, +x: U32) -> F(n, RT):
  match n:
    case 0n:
      0
    case 1n+p:
      RT{x, U32.add(x, 1), U32.add(x, 7)}

def pack(+n: Nat, x: F(n, RT)) -> Carrier<n>:
  Carrier{x, None{}}

def first(c: Carrier<0n>) -> U32:
  Carrier{x, tail} = c
  x

def give(c: Carrier<0n>) -> Carrier<0n>:
  c

def heat(+c: Carrier<0n>) -> U32:
  U32.add(first(give(c)), first(c))

def use(+n: Nat, c: Carrier<n>) -> U32:
  match n:
    case 0n:
      Carrier{x, tail} = c
      x
    case 1n+p:
      Carrier{x, tail} = c
      RT{a, b, c} = x
      U32.add(a, c)

def result() -> U32:
  +n = opaque_one()
  U32.add(heat(Carrier{U32.from_nat(n), None{}}), use(n, pack(n, make(n, 10))))

def main() -> U32:
  result()

#|29
