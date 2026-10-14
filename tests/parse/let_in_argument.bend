# a let is a Body, not a Term: an argument, a constructor field, a type
# argument and a Kind operand each hold a term, so a let there takes the
# parens the grammar gives a body ("(" Body ")"); each position below
# parses and checks (the snf substitutes the lets, so this pins the
# parser, not the printer)
import Base

type Box<A: Type> is Type:
  MkBox{x: A}

def use2(a: Nat, b: Nat) -> Nat:
  Nat.add(a, b)

def main(n: Nat) -> Nat:
  use2((a = n; a), 2n)

def fld(n: Nat) -> Box<Nat>:
  MkBox{(a = n; a)}

def typ(A: Type) -> Type:
  Box<(a = A; a)>

def knd(q: Quant) -> Type:
  Kind((k = q; k))

def bod(n: Nat, m: Nat) -> Nat:
  a = n
  use2(a, (b = m; b))

#|n => Nat.add(n, 2n)
