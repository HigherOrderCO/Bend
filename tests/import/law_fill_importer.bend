# a law its importer fills has no body while the file that declares it
# checks, and no order there helps: a live use in that file names the
# law, not the file order (#1782)
import Base
import ./law-fill/used.bend as L

def L.foo():
  0

#|SOME PROOFS FAIL
#|Error:
#|- expected : a filled definition (an unfilled law is a dead claim: live code cannot use it)
#|- observed : foo
#|Location: bar
#|10 | def bar():
#|11>|   foo
#|   |   ^^^
#|12 |
#|exit 1
