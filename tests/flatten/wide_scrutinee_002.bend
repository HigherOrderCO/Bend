type Nat is Data:
  Z{}
  S{p: Nat}

type R is Data:
  A{}
  B{x: Nat, y: Nat, z: Nat}
  C{x: Nat, y: Nat, z: Nat}

law main:
  for a: Nat
  for b: Nat
  for c: Nat
  for d: Nat
  for e: Nat
  R

def main(a, b, c, d, e):
  match a, b, c, d, e:
    case Z{}, b, Z{}, d, Z{}:
      A{}
    case S{x}, b, y, d, S{z}:
      B{x,y,z}
    case x, b, y, d, z:
      C{x,y,z}

#|\{Z: (b => \{Z: (d => \{Z: A{}; (e => C{Z{}, Z{}, e})}); (c => d => e => C{Z{}, c, e})}); S: (x => b => c => d => \{S: (z => B{x, c, z}); (e => C{S{x}, c, e})})}
