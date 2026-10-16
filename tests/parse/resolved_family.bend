# Applying family arguments must retain X's resolved datatype identity.
import resolved_a.bend as X
import resolved_b.bend as resolved_a

def nested() -> X.Box<X.Box<Quant>>:
  X.Wrap{X.Wrap{&1}}

#|All terms check.
