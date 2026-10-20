# A unary constructor tail must reuse its destination. The alternating
# workers also exercise tail transfers between different functions.
import Base

type Chain is Data:
  End{}
  Link{tail: Chain}

@unsafe
def even(n: Nat) -> Chain:
  match n:
    case 0n:
      End{}
    case 1n+p:
      t = odd(p)
      Link{t}

@unsafe
def odd(n: Nat) -> Chain:
  match n:
    case 0n:
      End{}
    case 1n+p:
      t = even(p)
      Link{t}

def length(t: Chain, n: Nat) -> Nat:
  match t:
    case End{}:
      n
    case Link{t}:
      length(t, Nat.add(n, 1n))

def main() -> IO(Unit):
  t = even!(256n)
  IO.print(Nat.show(length!(t, 0n)))

#|256
