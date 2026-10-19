# A & B is Sigma<&1, &1, ..>, which is Type; a list element asks for
# Data, so the shorthand pair fails. The note names the +Sigma spelling.

import Base

def f(xs: List<&2, U32 & U32>) -> List<&2, U32 & U32>:
  xs

#|SOME PROOFS FAIL
#|Error:
#|- expected : Data
#|- observed : Type
#|Location: f
#|5 |
#|6>| def f(xs: List<&2, U32 & U32>) -> List<&2, U32 & U32>:
#|  |                    ^^^^^^^^^
#|7 |   xs
#|Note: A & B is Sigma<&1, &1, ..>, which is Type. Write +Sigma<A, _ => B> for a pair that is Data.
#|exit 1
