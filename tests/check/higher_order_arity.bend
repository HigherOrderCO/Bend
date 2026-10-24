# A higher-order def called with too few arguments: the note names the
# missing parameter, even when its type is a function.

import Base

def h(g: (U32) -> U32) -> U32:
  g(1)

def main() -> U32:
  h

#|SOME PROOFS FAIL
#|Error:
#|- expected : U32
#|- observed : @g:(@_:U32 -> U32) -> U32
#|Location: main
#| 9 | def main() -> U32:
#|10>|   h
#|   |   ^
#|11 |
#|Note: h takes 1 argument, 0 given (missing g : @_:U32 -> U32).
#|exit 1
