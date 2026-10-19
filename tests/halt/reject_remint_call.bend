# descent rejects re-minted supply: a def call in the recursive
# argument could rebuild what the match consumed, so calls are never a
# descent -- f(grow(N{})) shrinks no column.
# ported from bend3: bare ctors, main returns the test's own type
type T is Type:
  C{f: T -> T}
  N{}

law grow:
  for t: T
  T

def grow(t):
  C{y => t}

law loop:
  for t: T
  T

def loop(t):
  match t:
    case C{f}:
      loop(f(grow(N{})))
    case t:
      t

law main:
  T

def main(): N{}

#|SOME PROOFS FAIL
#|Error:
#|- message  : loop's 1st argument, f(C{y => N{}}), is not smaller than t
#|Context:
#|- f : @_:T -> T
#|Location: loop
#|22 |     case C{f}:
#|23>|       loop(f(grow(N{})))
#|   |       ^^^^
#|24 |     case t:
#|Note: pass a part of t, or mark the def @unsafe to skip the termination check.
#|exit 1
