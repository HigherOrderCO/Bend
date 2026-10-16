# Selecting an operator must retain both plain and family head identities.
import resolved_a.bend as X
import resolved_b.bend as resolved_a

def family() -> {(&1 + &2 : X.Box<Quant>) == &1 : Quant}:
  {==}

def main() -> Quant:
  (&1 + &2 : X.Q)

#|&1
