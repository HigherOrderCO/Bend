# Regression for branch-local spare nodes in val_arms. The Put arm consumes the
# parked List Cons while boxing R2, but the runtime Skip arm must keep it for the
# rebuilt Cons instead of allocating and leaking one node per pass.
import Base

type R2 is Data:
  R2{a: U32, b: U32}

type Pick<-A: Data> is Data:
  Put{value: A}
  Skip{value: U32}

def generic(-A: Data, x: Pick<A>) -> Pick<A>:
  x

def make(n: Nat) -> List<Pick<R2>>:
  match n:
    case 0n:
      []
    case 1n+p:
      Skip{7} <> make(p)

def pass(xs: List<Pick<R2>>) -> List<Pick<R2>>:
  match xs:
    case []:
      []
    case x <> tail:
      generic(R2, x) <> tail

def rounds(n: Nat, xs: List<Pick<R2>>) -> List<Pick<R2>>:
  match n:
    case 0n:
      xs
    case 1n+p:
      rounds(p, pass(xs))

def length(xs: List<Pick<R2>>) -> Nat:
  match xs:
    case []:
      0n
    case x <> tail:
      (1n + length(tail) : Nat)

def main() -> IO(Unit):
  IO.print(Nat.show(length(rounds(1000n, make(1n)))))

#|1
