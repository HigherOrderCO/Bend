# #902: an instance re-entered while its own body is being checked is a
# self-call through another instance, and no column can show it decreasing
type Empty is Data:

type Unit is Data:
  Unit{}

type Key is Data:
  Key{}

type Bit is Data:
  False{}
  True{}

def bounce(~f: Unit -> Empty, u: Unit) -> Empty:
  f(u)

def loop(~k: Key, u: Unit) -> Empty:
  bounce(~loop(~k), u)

def boom() -> Empty:
  loop(~Key{}, Unit{})

def absurd(-A: Type, e: Empty) -> A:
  match e:

def false_theorem() -> {False{} == True{} : Bit}:
  absurd({False{} == True{} : Bit}, boom())

#|SOME PROOFS FAIL
#|Error:
#|- expected : a decreasing self-call (arguments are read left to right: each passed unchanged until one shrinks)
#|- observed : loop
#|Context:
#|- u : Unit
#|Location: bounce~inst
#|18 | def loop(~k: Key, u: Unit) -> Empty:
#|19>|   bounce(~loop(~k), u)
#|   |           ^^^^
#|20 | 
#|exit 1
