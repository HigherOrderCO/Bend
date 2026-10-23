# bend3's NSI ledger closed this gate arm by arm; bend refuses the
# same file at the descent wall: loop(f(y)) under the minted lambda is
# a whole self-call whose argument f(y) shrinks no column.
# ported from bend3: bare ctors, main returns the test's own type
type T is Type:
  C{f: T -> T}
  N{}

law apply1:
  for g: T -> T
  for v: T
  T

def apply1(g, v):
  g(v)

law loop:
  for t: T
  T

def loop(t):
  match t:
    case C{f}:
      apply1(y => loop(f(y)), C{z => z})
    case t:
      t

law main:
  T

def main(): N{}

#|SOME PROOFS FAIL
#|Error:
#|- message  : loop's 1st argument, f(y), is not smaller than t
#|Context:
#|- f : @_:T -> T
#|- y : T
#|Location: loop
#|23 |     case C{f}:
#|24>|       apply1(y => loop(f(y)), C{z => z})
#|   |                   ^^^^
#|25 |     case t:
#|Note: pass a part of t, or mark the def @unsafe to skip the termination check.
#|exit 1
