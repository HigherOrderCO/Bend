# @unsafe reaches a def through a constructor's field type: Box's field
# names boom, so Box relies on unsafe, and so does a def whose type names
# Box
import Base

@unsafe
def boom() -> Empty:
  boom()

type Box is Data:
  MkBox{e: {boom() == boom() : Empty}}

def uses(b: Box) -> Nat:
  0n

def sound(n: Nat) -> Nat:
  n

#|SOME PROOFS FAIL
#|Error: 3 defs rely on unsafe or foreign code:
#|defs that depend on @unsafe: boom, Box, uses
#|exit 1
