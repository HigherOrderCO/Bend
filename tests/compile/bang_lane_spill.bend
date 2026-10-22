# A bang's root and a fork kid run on a lane's STAK_LEN-word stack on the
# device. A call that finds it full moves the lane's stack words to a heap
# block under an exit frame, whose return moves them back, so non-tail
# recursion under ! goes as deep on the device as on the host (#1393, #1394).
# Before, 1023 List.append frames faulted, and so did a 365-fork spine beside
# a 3-fork one. walk's first spill comes at its second fork call, where the
# def still holds two words on the stack, and they must come back with it.
# tall's fork holds six 40-word records across its first call and hands one
# to each later call, so its frames add up to 449 words. A spill at the first
# call comes back near the top of the stack, and the later calls push onto
# it with no check in between, so the lane keeps room for the whole chain.
# With room for one frame instead, the second tall! faulted on CUDA.
import Base

def gen(n: Nat, +k: U32, acc: List<&2, U32>) -> List<&2, U32>:
  match n:
    case 0n:
      acc
    case 1n+p:
      gen(p, (k + 1 : U32), k <> acc)

def mix(xs: List<&2, U32>, +acc: U32) -> U32:
  match xs:
    case Nil{}:
      acc
    case Con{h, t}:
      mix(t, ((acc * 31 : U32) + h : U32))

def deep(+n: Nat) -> U32:
  mix(List.append(&2, U32, gen(n, 0, []), [7]), 0)

def spine(n: Nat, +i: U32) -> U32:
  match n:
    case 0n:
      0
    case 1n+p:
      a b = spine(p, (i + 1 : U32)) mix(List.append(&2, U32, gen(800n, i, []), gen(64n, i, [])), i)
      (a + b : U32)

type Q is Data:
  Q{a: U32, b: U32, c: U32, d: U32, e: U32}

def leaf(+k: U32) -> Q:
  Q{k, (k * 3 : U32), (k * 5 : U32), (k * 7 : U32), (k + 9 : U32)}

def sum(q: Q) -> U32:
  match q:
    case Q{a, b, c, d, e}:
      (((((a + b : U32) * 3 : U32) + c : U32) + ((d + e : U32) * 5 : U32) : U32) * 7 : U32)

def walk(n: Nat, +k: U32) -> U32:
  match n:
    case 0n:
      k
    case 1n+p:
      q b = leaf(k) walk(p, (k + 1 : U32))
      (((sum(q) + b : U32) * 31 : U32) + k : U32)

type P is Data:
  P{a: Q, b: Q, c: Q, d: Q, e: Q, f: Q, g: Q, h: Q}

def wide(+k: U32) -> P:
  P{leaf(k), leaf((k + 1 : U32)), leaf((k + 2 : U32)), leaf((k + 3 : U32)),
    leaf((k + 4 : U32)), leaf((k + 5 : U32)), leaf((k + 6 : U32)),
    leaf((k + 7 : U32))}

def flip(p: P, +k: U32) -> P:
  match p:
    case P{a, b, c, d, e, f, g, h}:
      P{h, g, f, e, d, c, b, leaf((sum(a) + k : U32))}

def fold(p: P) -> U32:
  match p:
    case P{a, b, c, d, e, f, g, h}:
      (((sum(a) + sum(b) : U32) + (sum(c) + sum(d) : U32) : U32) + ((sum(e) + sum(f) : U32) + (sum(g) + sum(h) : U32) : U32) : U32)

def tall(n: Nat, +k: U32) -> U32:
  match n:
    case 0n:
      k
    case 1n+p:
      u = wide(k)
      v = wide((k + 1 : U32))
      w = wide((k + 2 : U32))
      x = wide((k + 3 : U32))
      y = wide((k + 4 : U32))
      z = wide((k + 5 : U32))
      a b c d e f g = tall(p, (k + 1 : U32)) flip(u, k) flip(v, k) flip(w, k) flip(x, k) flip(y, k) flip(z, k)
      ((a * 31 : U32) + ((fold(b) + fold(c) : U32) + ((fold(d) + fold(e) : U32) + (fold(f) + fold(g) : U32) : U32) : U32) : U32)

def two(+n: Nat) -> U32:
  a b = spine(n, 0) spine(3n, 0)
  (a + b : U32)

def pair(+n: Nat) -> U32:
  a b = walk(n, 1) walk(3n, 0)
  (a + b : U32)

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print(U32.show(deep!(5000n)))
    IO.print(U32.show(two!(365n)))
    IO.print(U32.show(pair!(600n)))
    IO.print(U32.show(tall!(40n, 0)))
    IO.print(U32.show(tall!(40n, 1)))

#|1543298507
#|2063974529
#|1878402805
#|2730820976
#|2131216241
