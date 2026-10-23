# bend3 pinned that a non-size-increasing def is the only legal
# material source (loop mints 3 cells from 1, so loop(a(b)) had
# nothing to spend). bend has no material accounting: a(b) rebuilds
# through a HOAS application, no shrinking column, so loop(a(b)) is
# refused as non-decreasing -- while loop(p) itself descends fine.
# ported from bend3: bare ctors, main returns the test's own type
type T is Type:
  S{p: T}
  Z{}
  P{a: T -> T, b: T}

law loop:
  for t: T
  T

def loop(t):
  match t:
    case S{p}:
      loop(p)
    case Z{}:
      P{x => S{x}, Z{}}
    case P{a, b}:
      loop(a(b))

law main:
  T

def main(): Z{}

#|SOME PROOFS FAIL
#|Error:
#|- message  : loop's 1st argument, a(b), is not smaller than t
#|Context:
#|- a : @_:T -> T
#|- b : T
#|Location: loop
#|22 |     case P{a, b}:
#|23>|       loop(a(b))
#|   |       ^^^^
#|24 |
#|Note: pass a part of t, or mark the def @unsafe to skip the termination check.
#|exit 1
