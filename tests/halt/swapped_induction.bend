# the AI-loop's classic bad proof: an induction whose IH swaps the
# columns is not a descent. bend3 refused it with a type mismatch off
# its accepted swap; bend refuses the swap itself -- b sits in a's
# column with no subterm relation, so the IH is refused as
# non-decreasing. Never accepted in either dialect.
# ported from bend3: bare ctors S/Z over the test's own N; add is
# spelled on N so the equation stays inside one datatype
type N is Data:
  S{p: N}
  Z{}

law add:
  for a: N
  for b: N
  N

def add(a, b):
  match a:
    case Z{}:
      b
    case S{p}:
      S{add(p, b)}

law swapped:
  for a: N
  for b: N
  {add(a, b) == add(b, a) : N}

def swapped(a, b):
  match a:
    case Z{}:
      swapped(b, Z{})
    case S{p}:
      swapped(b, S{p})

law main:
  N

def main(): Z{}

#|SOME PROOFS FAIL
#|Error:
#|- message  : swapped's 1st argument, b, is not smaller than a
#|Context:
#|- b : N
#|Location: swapped
#|31 |     case Z{}:
#|32>|       swapped(b, Z{})
#|   |       ^^^^^^^
#|33 |     case S{p}:
#|Note: pass a part of a, or mark the def @unsafe to skip the termination check.
#|exit 1
