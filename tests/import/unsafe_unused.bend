# the verdict covers every def outside Base, imports' too: nothing here
# names the module's @unsafe def, yet it fails the verdict
import Base
import ./unsafe_lib.bend as L

def keep(n: Nat) -> Nat:
  L.safe(n)

#|SOME PROOFS FAIL
#|Error: 1 def relies on unsafe or foreign code:
#|- unsafe_lib.boom: @unsafe
#|exit 1
