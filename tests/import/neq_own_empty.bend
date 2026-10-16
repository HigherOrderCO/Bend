# the entry declares an inhabited Empty and offers it as the proof of an
# imported module's false != law. The law means the module's own Empty,
# so the offer is refused (#1331: it used to certify)
import ./neq_own_empty_lib.bend as L

type Empty is Data:
  Boom{}

def L.f_ne_g(x):
  e => Boom{}

#|SOME PROOFS FAIL
#|Error:
#|- expected : L.Empty
#|- observed : Empty
#|Context:
#|- x : L.T
#|- e : {L.A{} == L.A{} : L.T}
#|Location: L.f_ne_g
#| 9 | def L.f_ne_g(x):
#|10>|   e => Boom{}
#|   |        ^^^^^^
#|11 | 
#|exit 1
