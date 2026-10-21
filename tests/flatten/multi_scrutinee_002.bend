type Nat is Data:
  Z{}
  S{p: Nat}

type R is Data:
  A{x: Nat, y: Nat}
  B{x: Nat}
  C{x: Nat, y: Nat}

law main:
  for a: Nat
  for b: Nat
  R

def main(a, b):
  match a, b:
    case S{a}, S{b}:
      A{a,b}
    case Z{}, b:
      B{b}
    case a, b:
      C{a,b}

#|\{S: (a => \{S: (b => A{a, b}); (b => C{S{a}, b})}); Z: (b => B{b})}
