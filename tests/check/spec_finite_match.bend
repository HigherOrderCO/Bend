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

# an empty type after a finite one: each Bool's arm holds v's λ-match,
# which has no arms
law late:
  for ~b: Bool
  for ~v: Void
  Nat

def late(b, v):
  0n

# a Quant parameter: an arm per Quant
law mode:
  for ~q: Quant
  {q == q : Quant}

def mode(q):
  {==}

def Two(~b: Bool) -> Type:
  Pick(b)

# the fallback: Two(~b) at the bound b has no instance, so wrap's type
# can't go out and its arms name its instances bare
law wrap:
  for ~b: Bool
  for x: Two(~b)
  {x == x : Two(~b)}

def wrap(b, x):
  {==}

def Sz(~n: Nat) -> Type:
  Nat

# the fallback at a parameter that isn't finite: n goes out at an opaque
# constant, so Sz(~n) has an instance at it, but G binds n
law size:
  for ~n: Nat
  for ~b: Bool
  for x: Sz(~n)
  {x == x : Nat}

def size(n, b, x):
  {==}

# two Quant parameters: each of the outer arms holds a λ-match on q
law both:
  for ~p: Quant
  for ~q: Quant
  {p == p : Quant}

def both(p, q):
  {==}
def main() -> Nat:
  3n

#|3n
