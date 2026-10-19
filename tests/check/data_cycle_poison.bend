# red-team pin inverted: bend3 needed a poison walk (a function field
# anywhere in A -> B -> C nulls every member's table) to refuse sharing
# an A whose own fields are innocent. bend needs no poison: core is
# affine for EVERY type, so use2(a, a) refuses outright, closure field
# or not -- the wall the poison approximated is the default.
# ported from bend3: names resolve upward only, so the chain declares
# C2, B2, A2; ctor names are bare and globally unique
import Base

type C2 is Type:
  MkC2{f: U32 -> U32}
  EndC2{}
type B2 is Type:
  MkB2{c: C2}
  EndB2{}
type A2 is Type:
  MkA2{b: B2}
  EndA2{}

law mk:
  A2

def mk(): EndA2{}

law use2:
  for x: A2
  for y: A2
  U32

def use2(x, y): 0

law main:
  U32

def main():
  a = mk()
  use2(a, a)

#|SOME PROOFS FAIL
#|Error:
#|- message  : a is used 2 times (at 37:8, 37:11), but a plain binding is used at most once
#|Location: main
#|36 |   a = mk()
#|37>|   use2(a, a)
#|   |           ^
#|38 |
#|Note: a +a binding may be used many times if its type is Data; otherwise use it once (a function or another Type value cannot be copied).
#|exit 1
