# A def specialized at a finite parameter went to --verdict only as its
# instances, one per value: at Void, with none, skip was never checked
# (#1186). Now it also goes out as a λ-match on the parameter that names
# an instance at the law's type in each value's arm: skip's is λ{(,): λ{}},
# and same's has an arm per Bool. main prints 3n too and no gate runs
# --verdict to failure, so this documents the fix rather than guards it:
# by hand, --verdict fails with "- skip: no kernel def" if skip's λ-match
# stops going out.
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
