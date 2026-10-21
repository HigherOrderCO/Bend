type Nat is Data:
  Z{}
  S{p: Nat}

type R is Data:
  A{a: Nat, b: Nat}
  B{a: Nat}
  C{a: Nat, b: Nat}
  D{a: Nat, b: Nat}

law main:
  for a: Nat
  for b: Nat
  R

def main(a, b):
  match a, b:
    case S{S{a}}, S{b}:
      A{a,b}
    case S{S{a}}, Z{}:
      B{a}
    case S{a}, b:
      C{a,b}
    case a, b:
      D{a,b}

#|\{S: \{S: (a => \{S: (b => A{a, b}); Z: B{a}}); (_9 => b => C{_9, b})}; (a => b => D{a, b})}
