type Nat is Data:
  Z{}
  S{p: Nat}

type List<-Elem: Data> is Data:
  Nil{}
  Cons{h: Elem, t: List<Elem>}

law main:
  for A: Type
  for b: List<Nat>
  for c: List<Nat>
  List<Nat>

def main(A, b, c):
  match A, b, c:
    case A, Nil{}, b:
      Nil{}
    case A, Cons{c, d}, b:
      d

#|A => \{Nil: (c => Nil{}); Cons: (c => d => c => d)}
