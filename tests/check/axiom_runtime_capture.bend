# comp pin (check_axiom_runtime_capture): bend3 smuggled an axiom into
# runtime through a comp-lambda capture and died at erasure. bend's ~
# law tpl checks once, f an opaque constant; the ~ argument checks dead,
# where the axiom is a fine claim, and the instance tpl~0, tpl's body at
# the lambda, uses it live and is refused there. (the runtime-axiom wall
# itself is pinned by compile_axiom_runtime.)
import Base

law magic:
  U32

law tpl:
  for ~f: U32 -> U32
  for x: U32
  U32

def tpl(f, x):
  f(x)

law main:
  U32

def main():
  tpl(y => (y + magic : U32), 1)

#|SOME PROOFS FAIL
#|Error:
#|- expected : a filled definition (an unfilled law is a dead claim: live code cannot use it)
#|- observed : magic
#|Context:
#|- x : U32
#|Location: tpl~inst
#|23 | def main():
#|24>|   tpl(y => (y + magic : U32), 1)
#|   |                 ^^^^^
#|25 |
#|exit 1
