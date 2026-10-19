# a pure argument swap never shrinks: sw(b, S{p}) has b in a's column
# -- no subterm relation either way -- and sw(S{Z{}}, S{Z{}}) really
# loops. bend refuses it at the first non-EQ column.
# ported from bend3: bare ctors S/Z, main returns the test's own type
type N is Data:
  S{p: N}
  Z{}

law sw:
  for a: N
  for b: N
  N

def sw(a, b):
  match a:
    case Z{}:
      b
    case S{p}:
      sw(b, S{p})

law main:
  N

def main(): sw(S{Z{}}, S{Z{}})

#|SOME PROOFS FAIL
#|Error:
#|- message  : sw's 1st argument, b, is not smaller than a
#|Context:
#|- p : N
#|- b : N
#|Location: sw
#|18 |     case S{p}:
#|19>|       sw(b, S{p})
#|   |       ^^
#|20 |
#|Note: pass a part of a, or mark the def @unsafe to skip the termination check.
#|exit 1
