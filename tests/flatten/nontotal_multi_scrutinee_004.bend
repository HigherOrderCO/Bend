# red-team pin: NotTotal on a multi-scrutinee match with DISTINCT types
# names the inner column's type (elimination is column-major, left
# first): under x=Oy only Triy's Ay is covered.
# ported from bend3
type Bity is Data:
  Oy{}
  Iy{}

type Triy is Data:
  Ay{}
  By{}
  Cy{}

law f:
  for x: Bity
  for y: Triy
  Bity

def f(x, y):
  match x, y:
    case Oy{}, Ay{}:
      Oy{}
    case Iy{}, By{}:
      Iy{}

law main:
  Bity

def main(): f(Oy{}, Ay{})

#|SOME PROOFS FAIL
#|Error:
#|- message  : this match doesn't cover By, Cy
#|Location: f
#|19 | def f(x, y):
#|20>|   match x, y:
#|   |   ^^^^^^^^^^^
#|21 |     case Oy{}, Ay{}:
#|Note: add cases for `By` (pattern `By{}`), `Cy` (pattern `Cy{}`), or a catch-all case.
#|exit 1
