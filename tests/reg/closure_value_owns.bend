# A def used as a closure value must own its argument: hof applies pick
# through Clo~apply, while mint also captures the tree. Build a fresh tree
# each turn so a borrowed closure entry leaks heap even with the right sum.
# Warm 10k turns, then check 1M more: the C twin bounds H_BUMP growth by one
# allocator quantum, allowing free-list retention, not measuring live bytes.
# Both twins assert the sums; JS does not measure heap growth.
import Base

type Tr is Data:
  L{v: U32}
  N{a: Tr, b: Tr}

law build:
  for +d: Nat
  for +x: U32
  Tr

def build(d, x):
  match d:
    case 0n:
      L{x}
    case 1n+p:
      N{build(p, x), build(p, U32.add(x, 1))}

law pick:
  for t: Tr
  U32

def pick(t):
  match t:
    case L{v}:
      v
    case N{a, b}:
      U32.add(pick(a), pick(b))

law hof:
  for f: Tr -> U32
  for t: Tr
  U32

def hof(f, t):
  f(t)

law mint:
  for +t: Tr
  U32 -> U32

def mint(t):
  x => U32.add(pick(t), x)

law loop:
  for n   : Nat
  for +x  : U32
  for acc : U32
  U32

def loop(n, x, acc):
  match n:
    case 0n:
      acc
    case 1n+q:
      +t = build(3n, x)
      loop(q, x, U32.add(acc, U32.add(hof(pick, t), mint(t)(1))))

def heap.check(value: U32, expected: U32, before: U32) -> IO(U32):
  import "./closure_value_owns.c"
  import "./closure_value_owns.js"

def main() -> IO(Unit):
  do IO<Unit>:
    before : U32 <- heap.check(loop(10000n, 1, 0), 410000, 0)
    after : U32 <- heap.check(loop(1000000n, 1, 0), 41000000, before)
    IO.print("41000000")

#|41000000
