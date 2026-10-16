# X.Q.add selects &1, so the stated equality must fail.
import resolved_a.bend as X
import resolved_b.bend as resolved_a

def theorem() -> {(&1 + &2 : X.Q) == &2 : Quant}:
  {==}

#|Error:
#|- expected : &1
#|- observed : &2
#|Location: theorem
#|5 | def theorem() -> {(&1 + &2 : X.Q) == &2 : Quant}:
#|6>|   {==}
#|7 |
#|exit 1
