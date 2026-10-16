# Changing leading quantities must retain the selected datatype identity.
import resolved_a.bend as X
import resolved_b.bend as resolved_a

def bare() -> +X.Handle:
  X.Hold{}

def applied() -> +X.Handle<&1>:
  X.Hold{}

#|All terms check.
