type Nat is Data:
  Z{}
  S{p: Nat}

type R is Data:
  A{}
  B{x: Nat}
  C{x: Nat}
  D{x: Nat, y: Nat}

law main:
  for a: Nat
  for b: Nat
  for c: Nat
  R

def main(a, b, c):
  match a, b, c:
    case Z{}, Z{}, c:
      A{}
    case Z{}, S{x}, c:
      B{x}
    case S{x}, Z{}, c:
      C{x}
    case y, z, c:
      D{y, z}

#|\{Z: \{Z: (c => A{}); S: (x => c => B{x})}; S: (x => \{Z: (c => C{x}); (b => c => D{S{x}, b})})}
