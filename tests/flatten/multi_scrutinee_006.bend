type Nat is Data:
  Z{}
  S{p: Nat}

type R is Data:
  A{a: Nat, b: Nat}
  B{a: Nat, b: Nat}
  C{a: Nat, b: Nat}
  D{a: Nat, b: Nat, c: Nat}

law main:
  for a: Nat
  for b: Nat
  for c: Nat
  R

def main(a, b, c):
  match a, b, c:
    case S{a}, Z{}, S{b}:
      A{a,b}
    case S{a}, b, Z{}:
      B{a,b}
    case Z{}, S{a}, b:
      C{a,b}
    case a, b, c:
      D{a,b,c}

#|\{S: (a => \{Z: \{S: (b => A{a, b}); Z: B{a, Z{}}}; (b => \{Z: B{a, b}; (c => D{S{a}, b, c})})}); Z: \{S: (a => c => C{a, c}); (b => c => D{Z{}, b, c})}}
