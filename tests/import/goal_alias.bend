# a goal spells another file's names as this file does, through its
# alias, so it reads back as written: M.quad(x) unfolds to
# M.double(M.double(x)). once it printed the key, goal-alias/mod.double,
# and a hub package's 0x<hash>/json.double did not even lex (#965)
import Base
import ./goal-alias/mod.bend as M

def quad_id(x: Nat) -> {M.quad(x) == x : Nat}:
  {==}

#|SOME PROOFS FAIL
#|Error:
#|- message  : {==} needs both sides to compute to the same value
#|Context:
#|- x : Nat
#|Location: quad_id
#| 8 | def quad_id(x: Nat) -> {M.quad(x) == x : Nat}:
#| 9>|   {==}
#|   |   ^^^^
#|10 |
#|- left     : M.quad(x)  =>  M.double(M.double(x))
#|- right    : x  =>  x
#|exit 1
