# bend3's NSI joint-group acceptance spent both matched fields of an
# nsi call's result; bend has no material accounting and no computed
# match -- the fields come off the P9 column and a(b) shrinks nothing,
# so the shape is a plain non-decreasing self-call and refuses as one
import Base

type T9j is Type:
  C9{x: T9j -> T9j, y: T9j}
  P9{a: T9j -> T9j, b: T9j}
  W9{}

law unpack:
  for t: T9j
  T9j

def unpack(t):
  match t:
    case C9{x, y}:
      P9{x, y}
    case t:
      t

law loop:
  for t: T9j
  T9j

def loop(t):
  match t:
    case P9{a, b}:
      loop(a(b))
    case w:
      w

law main:
  U32

def main():
  0

#|SOME PROOFS FAIL
#|Error:
#|- message  : loop's 1st argument, a(b), is not smaller than t
#|Context:
#|- a : @_:T9j -> T9j
#|- b : T9j
#|Location: loop
#|29 |     case P9{a, b}:
#|30>|       loop(a(b))
#|   |       ^^^^
#|31 |     case w:
#|Note: pass a part of t, or mark the def @unsafe to skip the termination check.
#|exit 1
