type Nat is Data:
  Z{}
  S{p: Nat}

type R is Data:
  A{}
  B{a: Nat, b: Nat, c: Nat, d: Nat}
  C{a: Nat, b: Nat, c: Nat, d: Nat}

law main:
  for a: Nat
  for b: Nat
  for c: Nat
  for d: Nat
  R

def main(a, b, c, d):
  match a, b, c, d:
    case Z{}, Z{}, Z{}, Z{}:
      A{}
    case S{w}, x, y, z:
      B{w, x, y, z}
    case p, q, r, s:
      C{p, q, r, s}

#|\{Z: \{Z: \{Z: \{Z: A{}; (d => C{Z{}, Z{}, Z{}, d})}; (c => d => C{Z{}, Z{}, c, d})}; (b => c => d => C{Z{}, b, c, d})}; S: (w => b => c => d => B{w, b, c, d})}
