# A return type written with the do block's effect syntax: the error names
# the other spelling instead of only saying that IO is unknown.

import Base

def main() -> IO<Unit>:
  do IO<Unit>:
    IO.print("hello")

#|SOME PROOFS FAIL
#|Error:
#|- message  : a type: `IO<...>` is the do block's effect syntax; a return type is written `IO(...)`
#|Location: main
#|5 | 
#|6>| def main() -> IO<Unit>:
#|  |               ^^^^^^^
#|7 |   do IO<Unit>:
#|exit 1
