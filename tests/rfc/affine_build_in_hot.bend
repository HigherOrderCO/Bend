# An erased quantity keeps the generic constructor conservative: the same
# body must build both affine lists and Data lists that really are copied.
import Base

def build(-q: Quant, n: Nat, +i: U32, acc: List<q, U32>) -> List<q, U32>:
  match n:
    case 0n: acc
    case 1n+p: build(q, p, U32.add(i, 1), i <> acc)

def affine(n: Nat, +i: U32, acc: List<U32>) -> List<U32>:
  match n:
    case 0n: acc
    case 1n+p: affine(p, U32.add(i, 1), i <> acc)

def sum(-q: Quant, xs: List<q, U32>, acc: U32) -> U32:
  match xs:
    case Nil{}: acc
    case h <> t: sum(q, t, U32.add(acc, h))

def twice(+xs: List<&2, U32>) -> U32:
  U32.add(sum(&2, xs, 0), sum(&2, xs, 0))

def main() -> IO(Unit):
  do IO<Unit>:
    Unit <- IO.print(U32.show(sum(&1, affine(1000n, 0, Nil{}), 0)))
    Unit <- IO.print(U32.show(twice(build(&2, 1000n, 0, Nil{}))))
    IO.print(U32.show(sum(&1, build(&1, 1000n, 0, Nil{}), 0)))

#|499500
#|999000
#|499500
