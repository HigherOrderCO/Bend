# error quality: an empty match over a non-empty type names every
# uncovered constructor
import Base
type MB is Data:
  Tt{}
  Ff{}

law f:
  for b: MB
  U32

def f(b):
  match b:

law main:
  U32

def main(): f(Tt{})

#|SOME PROOFS FAIL
#|Error:
#|- message  : this match doesn't cover Tt, Ff
#|Location: f
#|12 | def f(b):
#|13>|   match b:
#|   |   ^^^^^^^^
#|14 | 
#|Note: add cases for `Tt` (pattern `Tt{}`), `Ff` (pattern `Ff{}`), or a catch-all case.
#|exit 1
