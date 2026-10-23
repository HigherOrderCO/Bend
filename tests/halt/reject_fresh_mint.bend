# descent rejects minting: the argument spends a fresh lambda each
# round (with f = id the call rebuilds C{y => y} forever) -- f(C{y =>
# y}) shrinks no column, so the self-call is refused.
# ported from bend3: bare ctors, main returns the test's own type
type T is Type:
  C{f: T -> T}
  N{}

law loop:
  for t: T
  T

def loop(t):
  match t:
    case C{f}:
      loop(f(C{y => y}))
    case t:
      t

law main:
  T

def main(): N{}

#|SOME PROOFS FAIL
#|Error:
#|- message  : loop's 1st argument, f(C{y => y}), is not smaller than t
#|Context:
#|- f : @_:T -> T
#|Location: loop
#|15 |     case C{f}:
#|16>|       loop(f(C{y => y}))
#|   |       ^^^^
#|17 |     case t:
#|Note: pass a part of t, or mark the def @unsafe to skip the termination check.
#|exit 1
