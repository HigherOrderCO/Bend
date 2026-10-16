# The arm's resolved constructor must remain in the scrutinee's datatype.
import resolved_a.bend as X
import resolved_b.bend as resolved_a

def identity() -> X.Bit -> X.Bit:
  \{X.Off: X.Off{}}

#|All terms check.
