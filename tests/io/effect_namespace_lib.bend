import Base

def value() -> IO(U32):
  import "./effect_namespace_lib.js"

#|SOME PROOFS FAIL
#|Error: 1 def relies on unsafe or foreign code:
#|defs that depend on foreign effects: value
#|exit 1
