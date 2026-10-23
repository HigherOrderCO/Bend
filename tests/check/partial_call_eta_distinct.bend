# l(U) and r(U) disagree on a fresh argument, so they do not convert,
# however a proof of {l == r} is given (none is): the inline {==} fails
# and leaves Box<r(U)> as it was (#1190)
type U is Data:
  U{}

def l(A: Type, B: Type) -> Type:
  A

def r(A: Type, B: Type) -> Type:
  B

type Box<-h: Type -> Type> is Type:
  Box{}

def G(-f: Type -> Type -> Type, -g: Type -> Type -> Type) -> Type:
  {f == g : Type -> Type -> Type} -> Box<g(U)>

def make() -> G(l, r):
  e => Box{}

def inline() -> Box<l(U)>:
  make()({==})

#|SOME PROOFS FAIL
#|Error:
#|- expected : l
#|- observed : r
#|Location: inline
#|22 | def inline() -> Box<l(U)>:
#|23>|   make()({==})
#|   |          ^^^^
#|24 | 
#|exit 1
