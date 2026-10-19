# Families, quantities, arms and operators retain the selected module.
# The written alias resolved_a names the second module throughout.
import resolved_a.bend as X
import resolved_b.bend as resolved_a

def selected_family() -> X.Box<X.Box<Quant>>:
  X.Wrap{X.Wrap{&1}}

def family() -> resolved_a.Box<resolved_a.Box<Quant>>:
  resolved_a.Wrap{resolved_a.Wrap{&2}}

def bare() -> +X.Handle:
  X.Hold{}

def applied() -> +X.Handle<&1>:
  X.Hold{}

def quantity() -> +resolved_a.Handle:
  resolved_a.Hold{}

def selected_identity() -> X.Bit -> X.Bit:
  \{X.Off: X.Off{}}

def identity() -> resolved_a.Bit -> resolved_a.Bit:
  \{resolved_a.Off: resolved_a.Off{}}

def family_operator() -> {(&1 + &2 : X.Box<Quant>) == &1 : Quant}:
  {==}

def operator() -> {(&1 + &2 : resolved_a.Q) == &2 : Quant}:
  {==}

def first() -> {X.value == &1 : Quant}:
  {==}

def second() -> {resolved_a.value == &2 : Quant}:
  {==}

def x_do() -> X.Id:
  do X.Id<>:
    return &1

def alias_do() -> resolved_a.Id:
  do resolved_a.Id<>:
    return &2

def main() -> Quant:
  (&1 + &2 : X.Q)

#|&1
