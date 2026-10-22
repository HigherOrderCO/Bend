# operator chains are ordinary calls under the affine core: a binder
# consumed by two operands is refused (bend3 priced the chain; bend
# walls it)
import Base

law f:
  for x: U32
  U32

def f(x):
  (x + x : U32)

law main:
  U32

def main():
  f(3)

#|SOME PROOFS FAIL
#|Error:
#|- message  : x appears 2 times (at 11:4, 11:8), but a plain binding is used at most once
#|Location: f
#|10 | def f(x):
#|11>|   (x + x : U32)
#|   |        ^
#|12 |
#|Note: a +x binding may be used many times if its type is Data; otherwise use it once (a function or another Type value cannot be copied).
#|exit 1
