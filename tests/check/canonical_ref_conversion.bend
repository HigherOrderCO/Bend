# conversion is CANONICAL at the ref head: two distinct defs with
# byte-identical case trees do NOT convert applied to an opaque var --
# a stuck dispatch keeps its own ref spelling.
# ported from bend3
import Base

type Nc is Data:
  Zc{}
  Sc{p: Nc}

law f:
  for t: Nc
  U32

def f(t):
  match t:
    case Zc{}:
      0
    case Sc{p}:
      1

law g:
  for t: Nc
  U32

def g(t):
  match t:
    case Zc{}:
      0
    case Sc{p}:
      1

law eq:
  for t: Nc
  {f(t) == g(t) : U32}

def eq(t):
  {==}

law main:
  U32

def main(): 0

#|SOME PROOFS FAIL
#|Error:
#|- message  : {==} needs both sides to compute to the same value
#|Context:
#|- t : Nc
#|Location: eq
#|37 | def eq(t):
#|38>|   {==}
#|   |   ^^^^
#|39 |
#|- left     : f(t)  =>  f(t)
#|- right    : g(t)  =>  g(t)
#|exit 1
