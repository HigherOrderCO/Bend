type Nat is Data:
  Z{}
  S{p: Nat}

type R is Data:
  A{a: Nat, b: Nat}
  B{a: Nat, b: Nat}
  C{a: Nat}
  D{a: Nat, b: Nat}
  E{a: Nat, b: Nat}

law main:
  for a: Nat
  for b: Nat
  R

def main(a, b):
  match a, b:
    case S{S{a}}, S{S{b}}:
      A{a,b}
    case S{S{a}}, S{b}:
      B{a,b}
    case S{a}, Z{}:
      C{a}
    case S{a}, b:
      D{a,b}
    case a, b:
      E{a,b}

#|\{S: \{S: (a => \{S: \{S: (b => A{a, b}); (_12 => B{a, _12})}; Z: C{S{a}}}); (_11 => \{Z: C{_11}; (b => D{_11, b})})}; (a => b => E{a, b})}
