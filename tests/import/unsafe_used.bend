# a def that names the module's @unsafe def relies on unsafe too: the
# verdict lists both
import Base
import ./unsafe_lib.bend as L

def innocent() -> Empty:
  L.boom()

def keep(n: Nat) -> Nat:
  L.safe(n)

#|SOME PROOFS FAIL
#|Error: 2 defs rely on unsafe or foreign code:
#|defs that depend on @unsafe: unsafe_lib.boom, innocent
#|exit 1
