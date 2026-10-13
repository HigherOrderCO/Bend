# a module with an @unsafe def: its own verdict names it (and nothing
# else), and so does an importer's, with every def of its own that uses it
import Base

@unsafe
def boom() -> Empty:
  boom()

def safe(n: Nat) -> Nat:
  n

#|SOME PROOFS FAIL
#|Error: 1 def relies on unsafe or foreign code:
#|defs that depend on @unsafe: boom
#|exit 1
