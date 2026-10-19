# a datatype whose field reaches a closure (through a nested datatype)
# is affine like everything else: over-using a Badd binder is rejected
# at the second use.
# ported from bend3
import Base

type Bxd is Type:
  Bd{f: U32 -> U32}

type Badd is Type:
  Wd{b: Bxd}

law idd:
  for x: U32
  U32

def idd(x): x

law mk:
  Badd

def mk(): Wd{Bd{idd}}

law use:
  for b: Badd
  U32

def use(b): 0

law main:
  U32

def main():
  b = mk()
  U32.add(use(b), use(b))

#|SOME PROOFS FAIL
#|Error:
#|- message  : b appears 2 times (at 35:15, 35:23), but a plain binding is used at most once
#|Location: main
#|34 |   b = mk()
#|35>|   U32.add(use(b), use(b))
#|   |                       ^
#|36 |
#|Note: a +b binding may be used many times if its type is Data; otherwise use it once (a function or another Type value cannot be copied).
#|exit 1
