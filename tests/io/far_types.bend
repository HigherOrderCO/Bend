# io (io_far_types): a module whose effect lays values of the module's
# own type, imported by reg_far_import from another directory. Alone it
# has no main: checked and interpreted only.
import Base

type Far is Data:
  Near{v: U32}
  Deep{l: Far, r: Far}

law far.make:
  U32 -> IO(Far)

def far.make(depth):
  import "./far_types.c"
  import "./far_types.js"

#|SOME PROOFS FAIL
#|Error: 1 def relies on unsafe or foreign code:
#|defs that depend on foreign effects: far.make
#|exit 1
