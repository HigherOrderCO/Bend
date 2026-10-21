# ragged matrix: a leading `Z{} b` default-b row closes its branch
# without touching column b (dominance), the rigid rows drive the rest.
type Nat is Data:
  Z{}
  S{p: Nat}

type R is Data:
  A{b: Nat}
  B{a: Nat, b: Nat}
  C{a: Nat}

law main:
  for a: Nat
  for b: Nat
  R

def main(a, b):
  match a, b:
    case Z{}, b:
      A{b}
    case a, S{b}:
      B{a,b}
    case a, Z{}:
      C{a}

#|\{Z: \{S: (b => A{S{b}}); Z: A{Z{}}}; (a => \{S: (b => B{a, b}); Z: C{a}})}
