# A call with one argument too few: the note says how many the def takes
# and names what is missing.

import Base

def add2(a: U32, b: U32) -> U32:
  U32.add(a, b)

def main() -> U32:
  add2(1)

#|SOME PROOFS FAIL
#|Error:
#|- expected : U32
#|- observed : @b:U32 -> U32
#|Location: main
#| 9 | def main() -> U32:
#|10>|   add2(1)
#|   |   ^^^^^^^
#|11 |
#|Note: add2 takes 2 arguments, 1 given (missing b : U32).
#|exit 1
