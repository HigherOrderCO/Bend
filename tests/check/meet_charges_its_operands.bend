# The meet is not a laundry for a live occurrence. infer-min checks both
# operands at the AMBIENT demand and adds their measures, so writing a
# variable twice inside meets costs two uses exactly as writing it twice
# plainly does. &2 is the meet's identity, so before this rule charged,
# `q <&> &2` handed back a free copy of `q`: the honest `P{q, q}` was
# rejected while `P{q <&> &2, q <&> &2}` checked clean, which duplicated
# a value the kind system calls non-Data (Quant infers at Type, and
# checking Quant against Data fails). Both spellings must now agree.

import Base

type P is Type:
  P{a: Quant, b: Quant}

law dup:
  for q: Quant
  P

def dup(q):
  P{q <&> &2, q <&> &2}

#|SOME PROOFS FAIL
#|Error:
#|- message  : q appears 2 times (at 20:5, 20:15), but a plain binding is used at most once
#|Location: dup
#|19 | def dup(q):
#|20>|   P{q <&> &2, q <&> &2}
#|   |               ^
#|21 |
#|Note: a +q binding may be used many times if its type is Data; otherwise use it once (a function or another Type value cannot be copied).
#|exit 1
