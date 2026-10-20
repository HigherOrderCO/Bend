# an assert declares a DEAD claim: a def between the assert and its
# fill cannot call the name live -- the fill must precede live use.
# ported from bend3: bend3 ratified the forward call (C27); bend pins
# the opposite (an unfilled assert is dead-only, book order rules)
type Nz is Data:
  Zn{}
  Sn{p: Nz}

law twice:
  for x : Nz
  Nz

law early:
  for x: Nz
  Nz

def early(x):
  twice(x)

def twice(x):
  Sn{Sn{x}}

law main:
  Nz

def main(): early(Zn{})

#|SOME PROOFS FAIL
#|Error:
#|- expected : a def written above its use (defs are checked in file order)
#|- observed : twice
#|Context:
#|- x : Nz
#|Location: early
#|17 | def early(x):
#|18>|   twice(x)
#|   |   ^^^^^
#|19 |
#|exit 1
