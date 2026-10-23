# strictness: a lone deferred binder consumes nothing -- mk(x)'s
# argument x is the lambda's own fresh binder, no column's subterm, so
# the self-call under the minted Sup is refused.
# ported from bend3: bare ctors, main returns the test's own type
type Ord is Type:
  Sup{f: Ord -> Ord}
  Z{}

law mk:
  for u: Ord
  Ord

def mk(u):
  match u:
    case Sup{f}:
      Sup{x => mk(x)}
    case u:
      u

law main:
  Ord

def main(): Z{}

#|SOME PROOFS FAIL
#|Error:
#|- message  : mk's 1st argument, x, is not smaller than u
#|Context:
#|- f : @_:Ord -> Ord
#|- x : Ord
#|Location: mk
#|15 |     case Sup{f}:
#|16>|       Sup{x => mk(x)}
#|   |                ^^
#|17 |     case u:
#|Note: pass a part of u, or mark the def @unsafe to skip the termination check.
#|exit 1
