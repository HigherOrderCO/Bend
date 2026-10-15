# an alias may not shadow a name: Nat.add is Base's, and through the
# alias Nat the module's add, so the name is refused (it once silently
# meant the module's)
import Base
import ./alias_lib.bend as Nat

def main() -> Nat:
  Nat.add(2n, 2n)

#|SOME PROOFS FAIL
#|Error:
#|- expected : an unambiguous name (the alias Nat shadows Nat.add)
#|- observed : '('
#|Location:
#|7 | def main() -> Nat:
#|8>|   Nat.add(2n, 2n)
#|  |          ^
#|9 |
#|exit 1
