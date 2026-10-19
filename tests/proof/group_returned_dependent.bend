import Base
type N is Data:
  Z{}
  S{pred: N}
type V is Data:
  V0{}
  V1{}
type U0 is Data:
  U0{}
type U1 is Data:
  U1{}
def Family(v: V) -> Type:
  match v:
    case V0{}: U0
    case V1{}: U1
law f:
  for n: N
  for x: V
  for value: Family(x)
  Family(x)
def g(n: N, x: V) -> Family(x) -> Family(x):
  value =>
    -dummy = {u => f(n,x,value) : Unit -> Family(x)}
    value
def f(n,x,value):
  match n:
    case Z{}: value
    case S{p}: g(p,x,value)
def main() -> Family(V1{}):
  f(S{Z{}},V1{},U1{})
def claim() -> {main() == U1{} : U1}: {==}

#|U1{}
