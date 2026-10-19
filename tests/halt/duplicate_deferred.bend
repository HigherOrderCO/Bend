# duplication through the deferred path: bend3 pinned x spent twice
# in f(x, x). bend's descent judgment crosses the minted lambda and
# still demands a shrinking column: f(x, x) is no subterm of t, so the
# self-call is refused as non-decreasing before any spend is measured.
# ported from bend3: bare ctors, main returns the test's own type
type T is Type:
  C{f: T -> T -> T}
  D{g: T -> T}
  N{}

law loop:
  for t: T
  T

def loop(t):
  match t:
    case C{f}:
      D{x => loop(f(x, x))}
    case t:
      t

law main:
  T

def main(): N{}

#|SOME PROOFS FAIL
#|Error:
#|- message  : loop's 1st argument, f(x, x), is not smaller than t
#|Context:
#|- f : @_:T -> @_:T -> T
#|- x : T
#|Location: loop
#|17 |     case C{f}:
#|18>|       D{x => loop(f(x, x))}
#|   |              ^^^^
#|19 |     case t:
#|Note: pass a part of t, or mark the def @unsafe to skip the termination check.
#|exit 1
