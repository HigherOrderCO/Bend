# usage pin: the checker counts unevaluated source syntax. Ty(z)
# forces the shared argument while checking the annotation; the wnf
# machine must not write cells into the source, or consume(t)'s
# occurrence of t vanishes and the double consume passes (the audit's
# d5 witness: accepted or rejected by evaluation order).
type One is Data:
  One{}
type Res is Type:
  Res{f: One -> One}
type Box is Type:
  Box{a: One, t: Res}

law consume: Res -> One
def consume(t): One{}

law Ty: Box -> Type
def Ty(x):
  match x:
    case Box{One{}, t}: Box

law duplicate: Res -> Box
def duplicate(t): (z => {z : Ty(z)})(Box{consume(t), t})

#|SOME PROOFS FAIL
#|Error:
#|- message  : t appears 2 times (at 22:50, 22:54), but a plain binding is used at most once
#|Location: duplicate
#|21 | law duplicate: Res -> Box
#|22>| def duplicate(t): (z => {z : Ty(z)})(Box{consume(t), t})
#|   |                                                      ^
#|23 |
#|Note: a +t binding may be used many times if its type is Data; otherwise use it once (a function or another Type value cannot be copied).
#|exit 1
