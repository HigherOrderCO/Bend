type Nat is Data:
  Z{}
  S{p: Nat}
type List<a, -Elem: Kind(a)> is Kind(a):
  Nil{}
  Cons{h: Elem, t: List<a, Elem>}
type R is Data:
  A{a: Nat, b: List<&2, Nat>}
  B{a: Nat, b: Nat, c: List<&2, Nat>}
  C{a: Nat}
  D{a: Nat, b: List<&2, Nat>}
  E{a: Nat, b: List<&2, Nat>}
  F{}

law main:
  for a: List<&2, Nat>
  R

def main(a):
  match a:
    case Cons{S{S{a}}, Cons{Z{}, b}}:
      A{a,b}
    case Cons{S{a}, Cons{b, c}}:
      B{a,b,c}
    case Cons{S{a}, Nil{}}:
      C{a}
    case Cons{Z{}, Cons{S{a}, b}}:
      D{a,b}
    case Cons{a, b}:
      E{a,b}
    case Nil{}:
      F{}

#|\{Cons: \{S: \{S: (a => \{Cons: \{Z: (b => A{a, b}); (_19 => b => B{S{a}, _19, b})}; Nil: C{S{a}}}); (_18 => \{Cons: (b => c => B{_18, b, c}); Nil: C{_18}})}; Z: \{Cons: \{S: (a => b => D{a, b}); (_20 => b => E{Z{}, Cons{_20, b}})}; (_17 => E{Z{}, _17})}}; Nil: F{}}
