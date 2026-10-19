# an error in an imported file spells names as that file does: its own
# bare, its import's through its alias; once it printed the keys,
# goal-alias/bad.quad_id and goal-alias/mod.double (#965)
import Base
import ./goal-alias/bad.bend as B

def main() -> Nat:
  B.quad(1n)

#|SOME PROOFS FAIL
#|Error:
#|- message  : {==} needs both sides to compute to the same value
#|Context:
#|- x : Nat
#|Location: quad_id
#| 8 | def quad_id(x: Nat) -> {quad(x) == x : Nat}:
#| 9>|   {==}
#|   |   ^^^^
#|10 |
#|- left     : quad(x)  =>  M.double(M.double(x))
#|- right    : x  =>  x
#|exit 1
