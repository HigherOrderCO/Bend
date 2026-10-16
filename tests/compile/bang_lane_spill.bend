# A bang's root and a fork kid run on a lane's STAK_LEN-word stack on the
# device. A call that finds it full moves the words below its def's own to a
# heap block under an exit frame, whose return moves them back, so non-tail
# recursion under ! goes as deep on the device as on the host (#1393, #1394).
# Before, 1023 List.append frames faulted, and so did a 365-fork spine beside
# a 3-fork one. walk's first spill comes at its second fork call, where the
# def still holds two words on the stack, and they must come back with it.
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

#|1543298507
#|2063974529
#|1878402805
