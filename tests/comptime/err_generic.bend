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
#|- message  : {==} needs both sides to compute to the same value
#|Location: add0
#|7 | def add0(~n: Nat) -> {Nat.add(n, 0n) == n : Nat}:
#|8>|   {==}
#|  |   ^^^^
#|9 |
#|- left     : Nat.add(add0~n, 0n)  =>  Nat.add(add0~n, 0n)
#|- right    : add0~n  =>  add0~n
#|exit 1
