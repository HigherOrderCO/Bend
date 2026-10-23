# two calls of one def on different arguments are not rigidly equal, so
# the conversion unfolds them and still fails when the sides differ
import Base

type Box is Data:
  Box{n: Nat}

def pow2(+n: Nat) -> Nat:
  Nat.pow(2n, n)

def boxed_at(+n: Nat) -> Box:
  Box{pow2(n)}

def r() -> {Box{pow2(3n)} == boxed_at(4n) : Box}:
  {==}

#|SOME PROOFS FAIL
#|Error:
#|- expected : Box{8n}
#|- observed : Box{16n}
#|Location: r
#|14 | def r() -> {Box{pow2(3n)} == boxed_at(4n) : Box}:
#|15>|   {==}
#|   |   ^^^^
#|16 | 
#|exit 1
