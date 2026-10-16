# Written resolved_a still names the second module; only identities skip lookup.
import resolved_a.bend as X
import resolved_b.bend as resolved_a

def family() -> resolved_a.Box<resolved_a.Box<Quant>>:
  resolved_a.Wrap{resolved_a.Wrap{&2}}

def quantity() -> +resolved_a.Handle:
  resolved_a.Hold{}

def identity() -> resolved_a.Bit -> resolved_a.Bit:
  \{resolved_a.Off: resolved_a.Off{}}

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

#|All terms check.
