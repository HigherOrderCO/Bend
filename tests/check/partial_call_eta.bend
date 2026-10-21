# a partly applied def is a function, as a bare def is: l(U) and r(U)
# convert when they agree on a fresh argument. once only bare defs got
# eta, so l == r held but l(U) == r(U) did not, and an inline {==}
# proof, merging the shared cells of l and r, changed what a Box<r(U)>
# read as: the inline proof passed and the named one failed (#1190)
type U is Data:
  U{}

def l(A: Type, B: Type) -> Type:
  A

def r(A: Type, B: Type) -> Type:
  A

type Box<-h: Type -> Type> is Type:
  Box{}

def G(-f: Type -> Type -> Type, -g: Type -> Type -> Type) -> Type:
  {f == g : Type -> Type -> Type} -> Box<g(U)>

def make() -> G(l, r):
  e => Box{}

def proof() -> {l == r : Type -> Type -> Type}:
  {==}

def inline() -> Box<l(U)>:
  make()({==})

def named() -> Box<l(U)>:
  make()(proof())

def partial() -> {l(U) == r(U) : Type -> Type}:
  {==}

def direct(x: Box<r(U)>) -> Box<l(U)>:
  x

#|ALL PROOFS CHECK
#|Use --verdict for mathematical validity.
