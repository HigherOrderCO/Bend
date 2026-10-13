# Nest<n> reaches Nest<1n+n> through Hold<True{}, n>, whose field is the
# family Cell: the walk of lay_of opens Hold with the index it meets, so
# Nest is a box. Opened with DUMMY, Cell stays stuck, Nest is laid flat
# and lay_of recurses over Nest<1n>, Nest<2n>, ... until the stack ends
import Base

def Cell(b: Bool, n: Nat) -> Data:
  match b:
    case False{}:
      U32
    case True{}:
      Nest<(1n + n : Nat)>

type Hold<-b: Bool, -n: Nat> is Data:
  Hold{value: Cell(b, n)}

type Nest<-n: Nat> is Data:
  End{}
  More{next: Hold<True{}, n>}

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
            case Hold{x}:
              U32.add(1, read(p, (1n + n : Nat), x))

def main() -> U32:
  read(9n, 0n, More{Hold{More{Hold{End{}}}}})

#|9
