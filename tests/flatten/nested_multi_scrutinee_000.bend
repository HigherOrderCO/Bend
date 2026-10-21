type Nat is Data:
  Z{}
  S{p: Nat}

type R is Data:
  A{x: Nat, y: Nat}
  B{x: Nat, y: Nat}
  C{x: Nat}
  D{x: Nat, y: Nat}

law main:
  for a: Nat
  for b: Nat
  R

def main(a, b):
  match a, b:
    case S{a}, S{S{b}}:
      A{a,b}
    case S{a}, S{b}:
      B{a,b}
    case S{a}, Z{}:
      C{a}
    case d, e:
      D{d,e}

#|\{S: (a => \{S: \{S: (b => A{a, b}); (_11 => B{a, _11})}; Z: C{a}}); (a => b => D{a, b})}
