# A do block written with the type syntax: the error names the other
# spelling instead of only expecting '<'.

import Base

def main() -> IO(Unit):
  do IO(Unit):
    IO.print("hello")

#|SOME PROOFS FAIL
#|Error:
#|- expected : an effect's arguments in `<...>` (a do block is `do IO<...>:`, and the type is written `IO(...)`)
#|- observed : '('
#|Location:
#|6 | def main() -> IO(Unit):
#|7>|   do IO(Unit):
#|  |        ^
#|8 |     IO.print("hello")
#|exit 1
