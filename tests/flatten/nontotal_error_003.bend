# error quality: a non-exhaustive match names the missing constructor.
# ported from bend3
type Ne3 is Data:
  Ze3{}
  Se3{p: Ne3}

law pred:
  for n: Ne3
  Ne3

def pred(n):
  match n:
    case Se3{p}:
      p

law main:
  Ne3

def main(): pred(Se3{Ze3{}})

#|SOME PROOFS FAIL
#|Error:
#|- message  : this match doesn't cover Ze3
#|Location: pred
#|11 | def pred(n):
#|12>|   match n:
#|   |   ^^^^^^^^
#|13 |     case Se3{p}:
#|Note: add a case for `Ze3` (pattern `Ze3{}`), or a catch-all case.
#|exit 1
