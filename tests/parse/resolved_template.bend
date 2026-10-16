# Borrowed type names retain caller identity through the callee's context.
import resolved_a.bend as X
import resolved_b.bend as resolved_a

def Q() -> Type:
  Quant

def Q.add(x: Quant, y: Quant) -> Quant:
  x

def F(x: Quant) -> Type:
  Quant

def F.add(x: Quant, y: Quant) -> Quant:
  x

def calc(~T: Type) -> Quant:
  (&1 + &2 : T)

def imported() -> {calc(~X.Q) == &1 : Quant}:
  {==}

def bare() -> {resolved_a.calc(~Q) == &1 : Quant}:
  {==}

def applied() -> {resolved_a.calc(~F(&0)) == &1 : Quant}:
  {==}

#|All terms check.
