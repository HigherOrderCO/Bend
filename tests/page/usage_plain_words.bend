# usage errors speak human: binder names print bare and the quantity
# prints as words (consumed more than once), never an internal key --
# church-two needs f twice, but a lambda binder is affine
import Base
type MNat is Data:
  MZ{}
  MS{pred: MNat}

type Term is Type:
  TLam{bod: Term -> Term}
  TApp{fun: Term, arg: Term}
  TVar{idx: MNat}

law c2:
  Term

def c2():
  TLam{f => TLam{x => TApp{f, TApp{f, x}}}}

#|SOME PROOFS FAIL
#|Error:
#|- message  : f is used 2 times (at 18:28, 18:36), but a plain binding is used at most once
#|Location: c2
#|17 | def c2():
#|18>|   TLam{f => TLam{x => TApp{f, TApp{f, x}}}}
#|   |                                    ^
#|19 |
#|Note: a +f binding may be used many times if its type is Data; otherwise use it once (a function or another Type value cannot be copied).
#|exit 1
