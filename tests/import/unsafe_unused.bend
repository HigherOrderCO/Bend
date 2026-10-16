# the verdict covers every def outside Base, imports' too: nothing here
# names the module's @unsafe def, yet it fails the verdict
import Base
import ./unsafe_lib.bend as L

def keep(n: Nat) -> Nat:
  L.safe(n)

#|SOME PROOFS FAIL
#|Error: 1 def relies on unsafe or foreign code:
#|defs that depend on @unsafe: unsafe_lib.boom
#|Hint: no law or proof uses these defs. Move them to a file this proof does not import, and the proofs will check: unsafe_lib.boom
#|exit 1
