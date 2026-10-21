# ragged matrix over three columns: the trailing catch-all row rides
# and dominates where it leads; every row is live.
type Nat is Data:
  Z{}
  S{p: Nat}

type R is Data:
  A{a: Nat, b: Nat, c: Nat}
  B{a: Nat, b: Nat, c: Nat}
  C{a: Nat, b: Nat}
  D{a: Nat, b: Nat, c: Nat}

law main:
  for a: Nat
  for b: Nat
  for c: Nat
  R

def main(a, b, c):
  match a, b, c:
    case S{a}, S{b}, S{c}:
      A{a,b,c}
    case S{a}, S{b}, c:
      B{a,b,c}
    case S{a}, b, Z{}:
      C{a,b}
    case a, b, c:
      D{a,b,c}

#|\{S: (a => \{S: (b => \{S: (c => A{a, b, c}); Z: B{a, b, Z{}}}); (b => \{Z: C{a, b}; (c => D{S{a}, b, c})})}); (a => b => c => D{a, b, c})}
