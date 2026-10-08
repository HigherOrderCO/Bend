# a template's body checks once at its definition, each ~ parameter an
# opaque constant of its type (a bodiless def: it never computes), so the
# body must hold for every closed instance. a proof that holds only when
# the instance computes fails there, as the same def without ~ would
import Base

def add0(~n: Nat) -> {Nat.add(n, 0n) == n : Nat}:
  {==}

def main() -> {Nat.add(3n, 0n) == 3n : Nat}:
  add0(~3n)

#|SOME PROOFS FAIL
#|Error:
#|- expected : Nat.add(add0~param0, 0n)
#|- observed : add0~param0
#|Location: add0
#|7 | def add0(~n: Nat) -> {Nat.add(n, 0n) == n : Nat}:
#|8>|   {==}
#|  |   ^^^^
#|9 |
#|exit 1
