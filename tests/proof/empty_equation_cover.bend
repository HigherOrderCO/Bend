# Kernel verification checks unreachable specialized equation columns,
# including evidence used in the result, nested fields and later columns.
import Base

law only_true:
  for ~b: Bool
  for ~h: {b == True{} : Bool}
  Nat

def only_true(b, h): 1n

law use_evidence:
  for ~b: Bool
  for ~h: {b == True{} : Bool}
  {b == True{} : Bool}

def use_evidence(b, h): h

law impossible_nat:
  for ~e: {1n == 2n : Nat}
  Nat

def impossible_nat(e): 7n

law later_column:
  for ~b: Bool
  for ~h: {b == True{} : Bool}
  for ~c: Bool
  Nat

def later_column(b, h, c): 2n

type Packet is Data:
  Packet{flag: Bool, count: Nat}

law impossible_packet:
  for ~e: {Packet{False{}, 1n} == Packet{False{}, 2n} : Packet}
  Nat

def impossible_packet(e): 8n

def IsTrue(b: Bool) -> Type:
  {b == True{} : Bool}

law aliased_equation:
  for ~b: Bool
  for ~h: IsTrue(b)
  Nat

def aliased_equation(b, h): 9n

# Equal labels must retain the actual specialized instance, not an absurd.
law reflexive_column:
  for ~h: {False{} == False{} : Bool}
  Nat

def reflexive_column(h): 5n

def flip(b: Bool) -> Bool:
  match b:
    case False{}: True{}
    case True{}: False{}

law flipped_equation:
  for ~b: Bool
  for ~h: {flip(b) == False{} : Bool}
  Nat

def flipped_equation(b, h): 12n

law both_true:
  for ~a: Bool
  for ~b: Bool
  for ~h: {Bool.and(a, b) == True{} : Bool}
  Nat

def both_true(a, b, h): 13n

law computed_nat:
  for ~h: {Nat.add(1n, 1n) == 3n : Nat}
  Nat

def computed_nat(h): 7n

law unequal_words:
  for ~h: {1 == 2 : U32}
  Nat

def unequal_words(h): 8n

def main() -> Nat: 3n

#|3n
