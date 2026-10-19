# four columns, the last a nested list: under the Sz2 head the last
# column is partially covered and the page names the Cl2 column.
# ported from bend3
type Nz2 is Data:
  Zz2b{}
  Sz2{p: Nz2}

type Lz2<-Elem: Data> is Data:
  Nl2{}
  Cl2{h: Elem, t: Lz2<Elem>}

type Rz2 is Data:
  Az2{x: Lz2<Nz2>, y: Nz2}
  Bz2{x: Nz2, y: Nz2, z: Nz2}
  Cz2{w: Nz2, x: Nz2, y: Nz2, z: Nz2}

law f:
  for a: Nz2
  for b: Lz2<Nz2>
  for c: Nz2
  for d: Lz2<Nz2>
  Rz2

def f(a, b, c, d):
  match a, b, c, d:
    case Zz2b{}, xs, b, Nl2{}:
      Az2{xs, b}
    case Sz2{n}, xs, b, Cl2{y, Nl2{}}:
      Bz2{n, b, y}
    case Sz2{n}, xs, b, Cl2{y, Cl2{x, xs2}}:
      Cz2{n, x, b, y}

law main:
  Rz2

def main(): f(Zz2b{}, Nl2{}, Zz2b{}, Nl2{})

#|SOME PROOFS FAIL
#|Error:
#|- message  : this match doesn't cover Cl2
#|Context:
#|- b : Lz2<Nz2>
#|- c : Nz2
#|Location: f
#|24 | def f(a, b, c, d):
#|25>|   match a, b, c, d:
#|   |   ^^^^^^^^^^^^^^^^^
#|26 |     case Zz2b{}, xs, b, Nl2{}:
#|Note: add a case for `Cl2` (or `case _:`).
#|exit 1
