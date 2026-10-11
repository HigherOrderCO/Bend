import Base
type Shape is Data:
  Rect{w: Nat, h: Nat}
  Circle{r: Nat}
def is_rect(s: Shape) -> Bool:
  match s:
    case Rect{w, h}:
      True{}
    case Circle{r}:
      False{}
law shape_t:
  for ~mk: Nat -> Shape
  for ~ok: @x: Nat -> {is_rect(mk(x)) == True{} : Bool}
  for x: Nat
  {is_rect(mk(x)) == True{} : Bool}
def shape_t(mk, ok, x):
  ok(x)

# an earlier deeper finite constructor still wins over a nullary one.
def Vec(n: Nat) -> Type:
  match n:
    case 0n:
      Unit
    case 1n+p:
      Nat & Vec(p)

type Choice is Type:
  Deep{v: Vec(400n)}
  Shallow{}

def is_deep(c: Choice) -> Bool:
  match c:
    case Deep{v}:
      True{}
    case Shallow{}:
      False{}

law choice_t:
  for ~mk: Nat -> Choice
  for ~ok: @x: Nat -> {is_deep(mk(x)) == True{} : Bool}
  for x: Nat
  {is_deep(mk(x)) == True{} : Bool}

def choice_t(mk, ok, x):
  ok(x)

#|ALL PROOFS CHECK
#|Use --verdict for mathematical validity.
