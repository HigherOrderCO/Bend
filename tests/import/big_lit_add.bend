# the entry declares a Nat.add that returns its second argument and fills
# the module's law with it: 300 is zero. The module's literal now names
# its own Nat.add, which does not exist, so the law is refused (#1331: it
# used to certify)
import ./big_lit_add_lib.bend as L

type Nat is Data:
  Zero{}
  Succ{pred: Nat}

def Nat.add(a: Nat, b: L.Nat) -> L.Nat:
  b

def L.big_is_zero():
  {==}

#|SOME PROOFS FAIL
#|Error:
#|- expected : a defined name
#|- observed : Nat.add
#|Location: big_is_zero
#|19 | law big_is_zero:
#|20>|   {is_zero(300n+Zero{}) == True{} : Bool}
#|   |            ^^^^^^^^^^^
#|21 | 
#|exit 1
