# a Base-free module with its own Nat. A big literal lowers to Nat.add,
# a name this file never declares; the sugar wrote it bare, so the entry
# could supply it (#1331). Alone, the name is simply missing
type Nat is Data:
  Zero{}
  Succ{pred: Nat}

type Bool is Data:
  True{}
  False{}

def is_zero(n: Nat) -> Bool:
  match n:
    case Zero{}:
      True{}
    case Succ{p}:
      False{}

law big_is_zero:
  {is_zero(300n+Zero{}) == True{} : Bool}

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
