# @unsafe is the deliberately inconsistent fragment, and it propagates: the
# verdict names every def of the file that relies on it, the @unsafe def
# itself and, transitively, each def whose body or type names one. sound
# names none, so it is not listed
import Base

@unsafe
def boom() -> Empty:
  boom()

def innocent() -> Empty:
  boom()

def by_type(e: {boom() == boom() : Empty}) -> Nat:
  0n

def sound(n: Nat) -> Nat:
  n

#|SOME PROOFS FAIL
#|Error: 3 defs rely on unsafe or foreign code:
#|defs that depend on @unsafe: boom, innocent, by_type
#|exit 1
