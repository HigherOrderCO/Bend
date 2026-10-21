type Nat is Data:
  Z{}
  S{p: Nat}

type R is Data:
  A{}
  B{x: Nat}
  C{x: Nat}

law main:
  for a: Nat
  R

def main(a):
  match a:
    case Z{}:
      A{}
    case S{S{a}}:
      C{a}
    case a:
      B{a}

#|\{Z: A{}; S: \{S: (a => C{a}); (_3 => B{S{_3}})}}
