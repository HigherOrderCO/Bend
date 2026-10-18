# A flat generic def's spin takes and returns its words at its
# instance's layouts (#1195): Bool.pick at Tint, V2 and U32 selects
# words, at a List it keeps its node. A self-call that changes the
# erased type (grow) keeps the generic box; one that keeps it (last)
# stays a loop on words.
import Base

type Tint is Data:
  Tint{r: F32, g: F32, b: F32}

type V2 is Data:
  V2{x: U32, y: U32}

type Dup<-A: Data> is Data:
  Dup{a: A, b: A}

def tr(t: Tint) -> U32:
  Tint{+r, +g, b} = t
  F32.to_u32((r + g : F32))

def vr(v: V2) -> U32:
  V2{+a, +b} = v
  (a * 10 + b : U32)

def grow(-A: Data, +n: Nat, +x: A, +s: U32) -> U32:
  match n:
    case 0n:
      s
    case 1n+p:
      grow(Dup<A>, p, Dup{x, x}, (s * 2 + 1 : U32))

def last(-A: Data, +n: Nat, +x: A, +y: A) -> A:
  match n:
    case 0n:
      y
    case 1n+p:
      last(A, p, y, x)

def leaf(+i: U32) -> U32:
  t = tr(Bool.pick(Tint, (i % 2 < 1 : U32), Tint{1.0, 2.0, 3.0}, Tint{U32.to_f32(i), 0.5, 0.0}))
  v = vr(Bool.pick(V2, (i % 2 < 1 : U32), V2{1, 2}, V2{i, 3}))
  u = Bool.pick(U32, (i % 2 < 1 : U32), 7, i)
  l = U32.from_nat(List.length(&1, U32, Bool.pick(List<U32>, (i % 2 < 1 : U32), [1, 2], [i])))
  g = grow(Tint, U32.to_nat((i % 4 : U32)), Tint{U32.to_f32(i), 2.0, 3.0}, i)
  w = tr(last(Tint, U32.to_nat((i % 3 : U32)), Tint{U32.to_f32(i), 1.0, 0.0}, Tint{4.0, 5.0, 6.0}))
  (t + v + u + l + g + w : U32)

def tree(+k: Nat, +i: U32) -> U32:
  match k:
    case 0n:
      leaf(i)
    case 1n+e:
      a b = tree(e, (i * 2 : U32)) tree(e, (i * 2 + 1 : U32))
      (a + b : U32)

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print(U32.show(tree!(6n, 0)))

#|22163
