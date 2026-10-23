# term-level forward references FAIL: a law filled below its use has
# no body yet: book order rules, and the refusal says so (#1782).
# ported from bend3: bend3 ratified forward refs (C27); bend pins the
# refusal (the descent wall needs order)
import Base

law main:
  U32

def main(): later

law later:
  U32

def later(): 0

#|SOME PROOFS FAIL
#|Error:
#|- expected : a def written above its use (defs are checked in file order)
#|- observed : later
#|Location: main
#| 9 | 
#|10>| def main(): later
#|   |             ^^^^^
#|11 |
#|exit 1
