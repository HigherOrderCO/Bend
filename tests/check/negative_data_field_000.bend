# negative recursive data plus contraction builds a closed proof of
# Empty (delta(DCon{f}) = f(DCon{f}) needs f twice) -- the attack
# spells as bare over-use of the field and linearity rejects it.
# ported from bend3
type Dm<-A: Type> is Type:
  DCon{f: Dm<A> -> A}

law delta:
  for -A: Type
  for d: Dm<A>
  A

def delta(A, d):
  match d:
    case DCon{f}:
      f(DCon{f})

law main:
  Type

def main(): Type

#|SOME PROOFS FAIL
#|Error:
#|- message  : f is used 2 times (at 16:7, 16:14), but a plain binding is used at most once
#|Context:
#|- A : Type
#|Location: delta
#|15 |     case DCon{f}:
#|16>|       f(DCon{f})
#|   |              ^
#|17 |
#|Note: a +f binding may be used many times if its type is Data; otherwise use it once (a function or another Type value cannot be copied).
#|Note: f is bound by a pattern: put the + on the matched parameter, +d.
#|exit 1
