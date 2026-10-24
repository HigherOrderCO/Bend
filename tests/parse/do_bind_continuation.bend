# error census: a trailing bind line (no continuation) is rejected -- a
# bind needs a rest to feed.
# ported from bend3: Base's Result, bend error page
import Base

law f:
  Result<&2, &2, Unit, U32>

def f():
  do Result<&2, &2, Unit, U32>:
    x : U32 <- Result.pure(&2, &2, Unit, U32, 1)

#|SOME PROOFS FAIL
#|Error:
#|- expected : a term
#|- observed : end of input
#|Location:
#|21 | #|exit 1
#|22>|
#|   | ^
#|exit 1
