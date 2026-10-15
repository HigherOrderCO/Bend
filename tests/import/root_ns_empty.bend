# the entry declares an inhabited Empty and offers it as the proof of an
# imported module's != law. The entry has its own namespace, so the
# module's bare Empty is not the entry's: the offer is refused (#1331)
import ./root_ns_empty_lib.bend as L

type Empty is Data:
  Boom{}

def L.f_ne_g(x):
  e => Boom{}

#|SOME PROOFS FAIL
#|Error:
#|- expected : a defined name
#|- observed : Empty
#|Context:
#|- x : T
#|- _ : {A{} == A{} : T}
#|Location: f_ne_g
#|14 |   for x: T
#|15>|   {f(x) != g(x) : T}
#|   |         ^^
#|16 | 
#|exit 1
