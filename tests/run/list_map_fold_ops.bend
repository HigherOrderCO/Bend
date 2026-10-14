# bend3's list_map_fold_ops intent -- map/filter/fold/any/all over a
# list and map/fold over a Map -- in bend's dialect. bend3 rode base
# comp (~) templates; those are ruled out whole here, and generic
# List.map is not affine-checkable, so each template call becomes a
# local first-order recursion at U32 (the walk bend3's splice would
# have minted). The first six checks are the original's; the seventh,
# a constant 1 in the port, folds the Map without mmul10.
import Base

law b2u:
  for b: Bool
  U32

def b2u(b):
  match b:
    case False{}:
      0
    case True{}:
      1

law dbl:
  for xs: List<&2, U32>
  List<&2, U32>

def dbl(xs):
  match xs:
    case Nil{}:
      Nil{}
    case h <> t:
      U32.mul(h, 2) <> dbl(t)

law keep_gt1:
  for xs: List<&2, U32>
  List<&2, U32>

law keep_gt1.at:
  for h: U32
  for r: List<&2, U32>
  for z: Bool
  List<&2, U32>

def keep_gt1.at(h, r, z):
  match z:
    case True{}:
      h <> r
    case False{}:
      r

def keep_gt1(xs):
  match xs:
    case Nil{}:
      Nil{}
    case h0 <> t:
      +h = {h0 : U32}
      keep_gt1.at(h, keep_gt1(t), U32.is_gt(h, 1))

law suma:
  for xs  : List<&2, U32>
  for acc : U32
  U32

def suma(xs, acc):
  match xs:
    case Nil{}:
      acc
    case h <> t:
      suma(t, U32.add(acc, h))

law anyeq2:
  for xs: List<&2, U32>
  Bool

law anyeq2.at:
  for r: Bool
  for z: Bool
  Bool

def anyeq2.at(r, z):
  match z:
    case True{}:
      True{}
    case False{}:
      r

def anyeq2(xs):
  match xs:
    case Nil{}:
      False{}
    case h <> t:
      anyeq2.at(anyeq2(t), U32.is_eq(h, 2))

law allpos:
  for xs: List<&2, U32>
  Bool

law allpos.at:
  for r: Bool
  for z: Bool
  Bool

def allpos.at(r, z):
  match z:
    case True{}:
      r
    case False{}:
      False{}

def allpos(xs):
  match xs:
    case Nil{}:
      True{}
    case h <> t:
      allpos.at(allpos(t), U32.is_gt(h, 0))

law mmul10:
  for m: Map<&2, U32>
  Map<&2, U32>

def mmul10(m):
  match m:
    case MTip{}:
      MTip{}
    case MLeaf{k, v}:
      MLeaf{k, U32.mul(v, 10)}
    case MNode{pos, lo, hi}:
      MNode{pos, mmul10(lo), mmul10(hi)}

law mfold:
  for m   : Map<&2, U32>
  for acc : U32
  U32

def mfold(m, acc):
  match m:
    case MTip{}:
      acc
    case MLeaf{k, v}:
      U32.add(acc, U32.add(v, U32.from_nat(String.length(k))))
    case MNode{pos, lo, hi}:
      mfold(hi, mfold(lo, acc))

law main.out:
  U32

def main.out():
  a = b2u(U32.is_eq(suma(dbl([1, 2, 3]), 0), 12))
  b = b2u(U32.is_eq(suma(keep_gt1([1, 2, 3]), 0), 5))
  c = b2u(U32.is_eq(suma([1, 2, 3], 100), 106))
  d = b2u(anyeq2([1, 2, 3]))
  e = b2u(allpos([1, 2, 3]))
  g = b2u(U32.is_eq(mfold(mmul10(Map.from_list(&2, U32, [("a", 1), ("b", 2)])), 0), 32))
  h = b2u(U32.is_eq(mfold(Map.from_list(&2, U32, [("a", 1), ("b", 2)]), 2), 7))
  U32.add(U32.add(U32.add(U32.mul(a, 1000000), U32.mul(b, 100000)), U32.add(U32.mul(c, 10000), U32.mul(d, 1000))), U32.add(U32.add(U32.mul(e, 100), U32.mul(g, 10)), h))

law main:
  IO(Unit)

def main():
  IO.print(U32.show(main.out()))

#|1111111
