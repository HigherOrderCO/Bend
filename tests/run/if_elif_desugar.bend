# the if/elif/else sugar is gone with the lambda-match-application
# ban: a computed condition rides a worker def matching its Bool, a
# param condition matches structurally, elif chains nest in the False
# arm of the worker -- the arithmetic pins the old sugar's meaning
# ported from bend3: conditions turn Bool (comparisons answer Bool,
# no U32 truthiness), re-tested binders ride + (affine core)
import Base

law g.hi:
  for z: Bool
  U32

def g.hi(z):
  match z:
    case True{}:
      20
    case False{}:
      30

law g.lo:
  for +x: U32
  for  z: Bool
  U32

def g.lo(x, z):
  match z:
    case True{}:
      10
    case False{}:
      g.hi(U32.is_lt(x, 6))

law g:
  for x0: U32
  U32

def g(x0):
  +x = {x0 : U32}
  g.lo(x, U32.is_lt(x, 3))

law pick01:
  for z: Bool
  U32

def pick01(z):
  match z:
    case True{}:
      1
    case False{}:
      0

law agree:
  for x: U32
  for v: U32
  U32

def agree(x, v):
  pick01(U32.is_eq(g(x), v))

law nest:
  for a: Bool
  for b: Bool
  U32

def nest(a, b):
  match a:
    case True{}:
      match b:
        case True{}:
          3
        case False{}:
          2
    case False{}:
      1

law main.out:
  U32

def main.out():
  (agree(0, 10) + agree(2, 10) + agree(3, 20) + agree(5, 20) + agree(6, 30) + agree(9, 30) + 10 * (nest(True{}, True{}) + nest(True{}, False{}) + nest(False{}, True{})) : U32)

law main:
  IO(Unit)

def main():
  IO.print(U32.show(main.out()))

#|66
