type Nat is Data:
  Z{}
  S{p: Nat}

type R is Data:
  A{}
  B{a: Nat, x: Nat, y: Nat}
  C{a: Nat, x: Nat, y: Nat}

law main:
  for a: Nat
  for b: Nat
  for c: Nat
  for d: Nat
  R

def main(a, b, c, d):
  match a, b, c, d:
    case a, Z{}, c, Z{}:
      A{}
    case a, S{x}, c, y:
      B{a,x,y}
    case a, x, c, y:
      C{a,x,y}

#|a => \{Z: (c => \{Z: A{}; (d => C{a, Z{}, d})}); S: (x => c => d => B{a, x, d})}
