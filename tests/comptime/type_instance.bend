# a template call in a type is the template's body at its arguments, no
# instance: Pick(~Nat) computes to Nat wherever it stands, and the same
# call in a body is an instance of it
import Base

def Pick(~T: Type) -> Type:
  T

def wrap(x: Pick(~Nat)) -> Pick(~Nat):
  x

# A partial instance in a body must keep its source call in dependent types.
def apply(~f: Quant -> Quant, x: Quant, y: Quant) -> Quant:
  f(x)

def same(-A: Type, f: A) -> {f == f : A}:
  {==}

def partial() -> {apply(~x => x) == apply(~x => x) : Quant -> Quant -> Quant}:
  same(Quant -> Quant -> Quant, apply(~x => x))

def partial_arg() -> {apply(~x => x, &1) == apply(~x => x, &1) : Quant -> Quant}:
  same(Quant -> Quant, apply(~x => x, &1))

def under_binder() -> {x => apply(~y => y, x) == x => apply(~y => y, x) : Quant -> Quant -> Quant}:
  same(Quant -> Quant -> Quant, x => apply(~y => y, x))

# Pruning the enclosing match must also preserve its arm's source call.
def pruned_parent() -> {\{Unit: apply(~x => x, &1); rest => apply(~x => x, &1)} == \{Unit: apply(~x => x, &1); rest => apply(~x => x, &1)} : Unit -> Quant -> Quant}:
  same(Unit -> Quant -> Quant, \{Unit: apply(~x => x, &1); rest => apply(~x => x, &1)})

def main() -> Pick(~Nat):
  wrap(3n)

#|3n
