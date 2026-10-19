# accumulator ladder, state 2 (+IH): the induction hypothesis
# rewrites the call away; what remains on the page is the
# accumulator-shift lemma's shape
# ported from bend3: the ~ comp param specializes into the ladder's
# own successor step (a recursive combinator cannot bind a closure:
# a function type owns no Copiable witness); statement flipped for
# bend's right-into-left rewrite
type MN is Data:
  Z{}
  S{p: MN}

law each:
  for n: MN
  for x: MN
  MN

def each(n, x):
  match n:
    case Z{}:
      S{x}
    case S{p}:
      each(p, S{x})

law plus1:
  for n: MN
  for x: MN
  MN

def plus1(n, x):
  match n:
    case Z{}:
      S{x}
    case S{p}:
      S{plus1(p, x)}

law spec_thm:
  for n: MN
  for x: MN
  {plus1(n, x) == each(n, x) : MN}

def spec_thm(n, x):
  match n:
    case Z{}:
      {==}
    case S{p}:
      %spec_thm(p, S{x}) : {S{plus1(p, x)} == _ : MN}
      {==}

#|SOME PROOFS FAIL
#|Error:
#|- message  : {==} needs both sides to compute to the same value
#|Context:
#|- p : MN
#|- x : MN
#|Location: spec_thm
#|46 |       %spec_thm(p, S{x}) : {S{plus1(p, x)} == _ : MN}
#|47>|       {==}
#|   |       ^^^^
#|48 |
#|- left     : S{plus1(p, x)}  =>  S{plus1(p, x)}
#|- right    : plus1(p, S{x})  =>  plus1(p, S{x})
#|exit 1
