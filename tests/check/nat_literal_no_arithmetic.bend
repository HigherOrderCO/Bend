# a literal is Base's Nat, never a file's own: without Base, 4097n is
# refused against this file's Nat
type Nat is Data:
  Zero{}
  Succ{pred: Nat}

def id(~n: Nat) -> Nat:
  n

def main() -> Nat:
  id(~4097n)

def claim() -> {id(~4097n) == 4097n : Nat}:
  {==}

#|SOME PROOFS FAIL
#|Error:
#|- expected : a declared constructor (Nat declares Zero, Succ)
#|- observed : 4097n
#|Location: main
#|10 | def main() -> Nat:
#|11>|   id(~4097n)
#|   |       ^^^^^
#|12 | 
#|exit 1
