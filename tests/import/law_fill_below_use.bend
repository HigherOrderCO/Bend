# an importer that fills a law below its own use of it can move the
# fill up, so the refusal names the file order (#1782)
import Base
import ./law-fill/alone.bend as L

def baz() -> U32:
  L.foo

def L.foo():
  0

#|SOME PROOFS FAIL
#|Error:
#|- expected : a def written above its use (defs are checked in file order)
#|- observed : L.foo
#|Location: baz
#|6 | def baz() -> U32:
#|7>|   L.foo
#|  |   ^^^^^
#|8 |
#|exit 1
