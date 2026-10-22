# Nest<n> reaches Nest<1n+n> only through Hold<True{}, n>, a second
# instance of Hold after Hold<False{}, n>: lay_of's walk opens both, so
# Nest is a box, and does not recurse over Nest<1n>, Nest<2n>, ...
import Base

def Cell(b: Bool, n: Nat) -> Data:
  match b:
    case False{}:
      U32
    case True{}:
      Nest<(1n + n : Nat)>

type Hold<-b: Bool, -n: Nat> is Data:
  Hold{value: Cell(b, n)}

type Two<-A: Data, -B: Data> is Data:
  Two{a: A, b: B}

type Nest<-n: Nat> is Data:
  End{}
  More{next: Two<Hold<False{}, n>, Hold<True{}, n>>}

def read(k: Nat, n: Nat, t: Nest<n>) -> U32:
  match k:
    case 0n:
      0
    case 1n+p:
      match t:
        case End{}:
          7
        case More{next}:
          match next:
            case Two{a, b}:
              match b:
                case Hold{x}:
                  U32.add(1, read(p, (1n + n : Nat), x))

def main() -> U32:
  read(9n, 0n, More{Two{Hold{3}, Hold{More{Two{Hold{4}, Hold{End{}}}}}}})

#|9
