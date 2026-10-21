type Nat is Data:
  Z{}
  S{p: Nat}

type R is Data:
  A{}
  B{x: Nat, y: Nat}
  C{x: Nat, y: Nat}

law main:
  for a: Nat
  for b: Nat
  for c: Nat
  for d: Nat
  R

def main(a, b, c, d):
  match a, b, c, d:
    case Z{}, b, c, Z{}:
      A{}
    case S{x}, b, c, y:
      B{x,y}
    case x, b, c, y:
      C{x,y}

#|\{Z: (b => c => \{Z: A{}; (d => C{Z{}, d})}); S: (x => b => c => d => B{x, d})}
