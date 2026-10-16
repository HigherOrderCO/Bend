# the entry declares a Bool.or onto an inhabited type and offers it as the
# proof of an imported module's false || law. The law means the module's
# own Bool.or, so the offer is refused
import ./or_own_bool_lib.bend as L

type Unit is Data:
  U{}

def Bool.or(a: L.B, b: L.B) -> Type:
  Unit

def L.l(x):
  U{}

#|SOME PROOFS FAIL
#|Error:
#|- expected : L.Empty
#|- observed : Unit
#|Context:
#|- x : L.B
#|Location: L.l
#|12 | def L.l(x):
#|13>|   U{}
#|   |   ^^^
#|14 | 
#|exit 1
