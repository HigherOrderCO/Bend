# calls of one def with different arguments are not converted before
# they unfold: the conversion still fails when the unfolded sides differ
import Base

type Box is Data:
  Box{n: Nat}

def d1(+n: Nat) -> Box:
  Box{Nat.add(n, 8n)}

def r() -> {d1(2n) == Box{11n} : Box}:
  {==}

#|SOME PROOFS FAIL
#|Error:
#|- expected : Box{10n}
#|- observed : Box{11n}
#|Location: r
#|11 | def r() -> {d1(2n) == Box{11n} : Box}:
#|12>|   {==}
#|   |   ^^^^
#|13 | 
#|exit 1
