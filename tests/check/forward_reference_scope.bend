# bend3's C27 let a def between an assert and its fill call the
# asserted name; bend refuses by design -- a def may not use a
# demanded name (only asserts may); the refusal names the file order
import Base

law foo:
  for x: U32
  U32

law bar:
  U32

def bar():
  foo(1)

def foo(x):
  x

law main:
  U32

def main():
  0

#|SOME PROOFS FAIL
#|Error:
#|- expected : a def written above its use (defs are checked in file order)
#|- observed : foo
#|Location: bar
#|13 | def bar():
#|14>|   foo(1)
#|   |   ^^^
#|15 |
#|exit 1
