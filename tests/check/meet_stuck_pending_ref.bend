# book_valid walks the tlds in order, so a Ref to a declared but not yet
# filled def is stuck. a meet that decided itself against such a Ref would
# read one normal form while the def pends and another once it is filled,
# and nothing rechecks the evidence: 'q <&> &1' reducing to '&1' here would
# hand the finished book a closed proof of {&0 == &1 : Quant}. the meet
# stays stuck instead, so the Rfl is rejected where it is written.
import Base

law q:
  Quant

law thm:
  {q <&> &1 == &1 : Quant}

def thm():
  {==}

def q():
  &0

#|SOME PROOFS FAIL
#|Error:
#|- message  : {==} needs both sides to compute to the same value
#|Location: thm
#|15 | def thm():
#|16>|   {==}
#|   |   ^^^^
#|17 |
#|- left     : q <&> &1  =>  q <&> &1
#|- right    : &1  =>  &1
#|exit 1
