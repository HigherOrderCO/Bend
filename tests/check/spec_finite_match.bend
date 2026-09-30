# a def specialized at a finite parameter went to --verdict once per
# value, and --verdict trusted that those instances cover the law: at
# Void, with none, skip went out zero times and passed unchecked. Now it
# goes out once, a λ-match on the parameter whose arms are its instances'
# bodies, and its type a λ-match on it too, so the kernel checks the
# cover: skip's is λ{(,): λ{}}, and same's has an arm per Bool (#1186)
import Base

type Void is Data:

law skip:
  for ~v: Void
  for n: Nat
  Nat

def skip(v, n):
  n

law same:
  for ~b: Bool
  {b == b : Bool}

def same(b):
  {==}

def grade(b: Bool) -> Quant:
  match b:
    case False{}:
      &2
    case True{}:
      &1

def Payload(b: Bool) -> Kind(grade(b)):
  match b:
    case False{}:
      Nat
    case True{}:
      Array<U32>

law look:
  for ~b: Bool
  for p: Payload(b)
  Nat

def look(b, p):
  3n

def Pick(b: Bool) -> Type:
  match b:
    case False{}:
      Nat
    case True{}:
      Bool

# x's type names b, so x needs b closed: b goes out at each Bool
law pick:
  for ~b: Bool
  for ~x: Pick(b)
  {x == x : Pick(b)}

def pick(b, x):
  {==}

def main() -> Nat:
  3n

#|3n
