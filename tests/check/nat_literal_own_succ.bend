# a literal is Base's Nat, never a file's own: 1n is refused against this
# file's Nat, whose Succ holds an Empty, so no closed Empty comes out (#941)
type Empty is Data:

type Nat is Data:
  Succ{e: Empty}

def extract(n: Nat) -> Empty:
  match n:
    case Succ{e}:
      e

def boom() -> Empty:
  extract(1n)

#|SOME PROOFS FAIL
#|Error:
#|- expected : a declared constructor (Nat declares Succ)
#|- observed : 1n
#|Location: boom
#|13 | def boom() -> Empty:
#|14>|   extract(1n)
#|   |           ^^
#|15 | 
#|exit 1
