# a first-class match's empty tail keeps the match span, so a missing
# constructor points at the match that omitted it
type Bool is Data:
  True{}
  False{}
def bad() -> Bool -> Bool:
  \{True: True{}}

#|SOME PROOFS FAIL
#|Error:
#|- message  : this match doesn't cover False
#|Location: bad
#|6 | def bad() -> Bool -> Bool:
#|7>|   \{True: True{}}
#|  |   ^^^^^^^^^^^^^^^
#|8 | 
#|Note: add a case for `False` (pattern `False{}`), or a catch-all case.
#|exit 1
