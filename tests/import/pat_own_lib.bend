# a Base-free module with its own Nat and List: `case 0n:`, `case 1n+k:` and
# `case []:` match THIS module's constructors. Before, they read bare Zero and
# Nil, and failed with "unknown: Zero"
type Nat is Data:
  Zero{}
  Succ{p: Nat}

type List<A: Data> is Data:
  Nil{}
  Con{h: A, t: List<A>}

def pred(n: Nat) -> Nat:
  match n:
    case 0n:
      0n
    case 1n+k:
      k

def hd(xs: List<Nat>) -> Nat:
  match xs:
    case []:
      7n
    case x <> _:
      x

def run() -> Nat:
  pred(hd([3n, 4n]))

def main() -> Nat:
  run()

#|2n
