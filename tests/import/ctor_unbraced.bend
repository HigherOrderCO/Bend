# an imported constructor named through its alias but without braces
# gets the hint a local one gets (it once said only "a pattern")
import Base
import ./ctor_unbraced_lib.bend as C

def red(c: C.Color) -> Bool:
  match c:
    case C.Red:
      True{}
    case C.Green{}:
      False{}

def main() -> Bool:
  red(C.Green{})

#|SOME PROOFS FAIL
#|Error:
#|- message  : a braced constructor pattern (C.Red is a constructor: write C.Red{})
#|Location:
#|7 |   match c:
#|8>|     case C.Red:
#|  |          ^^^^^
#|9 |       True{}
#|exit 1
