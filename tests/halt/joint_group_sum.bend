# bend3 pinned that its NSI group rule may not pool a multi-argument
# call's columns (grow shrinks only the SUM, so More's field x is no
# smaller than column a -- accepting loop inhabits Empty). bend has no
# group rule at all: x comes off the fallback's field, no smaller than
# column a, so the self-call is refused as non-decreasing outright.
# ported from bend3: bare ctors, Empty spelled as the base Empty
import Base

type T is Data:
  S{p: T}
  Z{}

type Step is Data:
  More{x: T}

law grow:
  for a: T
  for fallback: Step
  for b: T
  Step

def grow(a, fallback, b):
  match b:
    case S{q}:
      More{a}
    case z:
      fallback

law loop:
  for a: T
  for fallback: Step
  for b: T
  Empty

def loop(a, fallback, b):
  match fallback:
    case More{x}:
      loop(x, grow(a, More{Z{}}, b), Z{})

law main:
  Empty

def main(): loop(Z{}, More{Z{}}, S{Z{}})

#|SOME PROOFS FAIL
#|Error:
#|- message  : loop's 1st argument, x, is not smaller than a
#|Context:
#|- a : T
#|- x : T
#|- b : T
#|Location: loop
#|37 |     case More{x}:
#|38>|       loop(x, grow(a, More{Z{}}, b), Z{})
#|   |       ^^^^
#|39 |
#|Note: pass a part of a, or mark the def @unsafe to skip the termination check.
#|exit 1
