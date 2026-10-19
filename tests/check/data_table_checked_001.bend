# the twin surface, carried whole: an inequality field ({0 != 0 : U32})
# is a FUNCTION into Empty from parse time -- bend3 derived a null Data
# table over the desugared field and refused the duplication; bend is
# affine for every type and refuses the same double spend with the same
# verdict, no table needed. The {a != b : T} sugar itself carries.
# ported from bend3: the let-then-match bodies route through helper
# defs (match heads a def body only); ctor names bare and unique
import Base

type Box6 is Type:
  MkBox6{proof: {0 != 0 : U32}}

law delta.fin:
  for first : {0 != 0 : U32}
  for b     : Box6
  Empty

def delta.fin(first, b):
  match b:
    case MkBox6{second}:
      first({==})

law delta.go:
  for a: Box6
  for b: Box6
  Empty

def delta.go(a, b):
  match a:
    case MkBox6{first}:
      delta.fin(first, b)

law delta:
  for box: Box6
  Empty

def delta(box):
  delta.go(box, box)

law main:
  Empty

def main(): delta(MkBox6{argument => delta(MkBox6{argument})})

#|SOME PROOFS FAIL
#|Error:
#|- message  : box appears 2 times (at 38:12, 38:17), but a plain binding is used at most once
#|Location: delta
#|37 | def delta(box):
#|38>|   delta.go(box, box)
#|   |                 ^^^
#|39 |
#|Note: a +box binding may be used many times if its type is Data; otherwise use it once (a function or another Type value cannot be copied).
#|exit 1
