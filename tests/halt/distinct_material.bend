# bend3's distinctness pin meets a wall one rule earlier in bend:
# the self-call's argument bod(a2) is a HOAS application, not a
# shrinking column, so infer-app refuses the recursion outright (the
# computed-match spelling is gone with the ban; the beta row is a
# nested Cal{Lam{bod}, a2} pattern).
# ported from bend3: bare ctors, Num loses its U32 payload (unused)
type Term is Type:
  Lam{bod: Term -> Term}
  Cal{fun: Term, arg: Term}
  Num{}

law wnf:
  for t: Term
  Term

def wnf(t):
  match t:
    case Cal{Lam{bod}, a2}:
      wnf(bod(a2))
    case Cal{f, a2}:
      Cal{f, a2}
    case t:
      t

law main:
  Term

def main(): Num{}

#|SOME PROOFS FAIL
#|Error:
#|- message  : wnf's 1st argument, bod(a2), is not smaller than t
#|Context:
#|- bod : @_:Term -> Term
#|- a2  : Term
#|Location: wnf
#|18 |     case Cal{Lam{bod}, a2}:
#|19>|       wnf(bod(a2))
#|   |       ^^^
#|20 |     case Cal{f, a2}:
#|Note: pass a part of t, or mark the def @unsafe to skip the termination check.
#|exit 1
