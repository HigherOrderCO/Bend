# NOFK is transitive: a retained function that reaches a fork through
# another definition must not be marked no-fork. A recursive function
# with no reachable fork must keep its no-fork mark.
# The opaque C/JS effect keeps both function values in the emitted
# program. Native code asserts their actual descriptor marks and drops
# both values; JS checks that both retained values are functions.
# Removing transitive propagation while keeping direct fork detection
# makes the native assertion fail, although the old numeric-only test
# still prints 22. This checks metadata, not scheduler growth or speed,
# and does not independently cover the synthetic Clo~apply graph edge.
import Base

law leap:
  for +n: Nat
  U32

def leap(n):
  match n:
    case 0n:
      1
    case 1n+p:
      U32.add(1, leap(p))

law fork_pair:
  for +n: Nat
  U32

def fork_pair(n):
  a b = leap(n) leap(n)
  U32.add(a, b)

law wrap:
  for +n: Nat
  U32

def wrap(n):
  fork_pair(n)

law main.out:
  U32

def main.out():
  wrap(10n)

def flags(par: Nat -> U32, seq: Nat -> U32) -> IO(Unit):
  import "./nofk_closure.c"
  import "./nofk_closure.js"

law main:
  IO(Unit)

def main():
  do IO<Unit>:
    Unit <- flags(n => wrap(n), n => leap(n))
    IO.print(U32.show(main.out()))

#|22
