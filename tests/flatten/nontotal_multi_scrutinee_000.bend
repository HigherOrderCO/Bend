# multi-scrutinee coverage walks nested patterns: the first column's
# Cons head never covers Zz0 in its element, and the page names it.
# ported from bend3
type Nz0 is Data:
  Zz0{}
  Sz0{p: Nz0}

type Lz0<-Elem: Data> is Data:
  Nl0{}
  Cl0{h: Elem, t: Lz0<Elem>}

type Rz0 is Data:
  Az0{a: Nz0, b: Lz0<Nz0>, c: Nz0, d: Lz0<Nz0>}
  Bz0{a: Nz0, b: Lz0<Nz0>, c: Nz0, d: Lz0<Nz0>}
  Cz0{b: Lz0<Nz0>, c: Nz0, d: Lz0<Nz0>}
  Dz0{a: Nz0, b: Lz0<Nz0>}
  Ez0{a: Nz0, b: Lz0<Nz0>}
  Fz0{}

law f:
  for a: Lz0<Nz0>
  for b: Lz0<Nz0>
  Rz0

def f(a, b):
  match a, b:
    case Cl0{Sz0{a}, b}, Cl0{Sz0{c}, d}:
      Az0{a, b, c, d}
    case Cl0{Sz0{a}, b}, Cl0{c, d}:
      Bz0{a, b, c, d}
    case Cl0{Zz0{}, b}, Cl0{Sz0{c}, d}:
      Cz0{b, c, d}
    case Cl0{a, b}, Nl0{}:
      Dz0{a, b}
    case Nl0{}, Cl0{a, b}:
      Ez0{a, b}
    case Nl0{}, Nl0{}:
      Fz0{}

law main:
  Rz0

def main(): f(Nl0{}, Nl0{})

#|SOME PROOFS FAIL
#|Error:
#|- message  : this match doesn't cover Zz0
#|Context:
#|- b : Lz0<Nz0>
#|Location: f
#|25 | def f(a, b):
#|26>|   match a, b:
#|   |   ^^^^^^^^^^^
#|27 |     case Cl0{Sz0{a}, b}, Cl0{Sz0{c}, d}:
#|Note: add `case Zz0{}:` (or `case _:`).
#|exit 1
