# A template parameter cannot name a constructor arm.
type Bit is Data:
  Off{}

def bad(~f: Quant -> Quant) -> Bit -> Bit:
  \{f: Off{}}

#|SOME PROOFS FAIL
#|Error:
#|- expected : a constructor of Bit (missing, or already matched)
#|- observed : \{f: Off{}}
#|Location: bad
#|5 | def bad(~f: Quant -> Quant) -> Bit -> Bit:
#|6>|   \{f: Off{}}
#|  |   ^^^^^^^^^^^
#|7 |
#|exit 1
