# proof_userland_tactics: the laws. The human states them; PROOF.bend
# must prove them. pay_within_budget is the real, quantified claim;
# le_succ_r is the classic wall lemma (a stuck x cannot be stripped).
# A parameter is + when a proof spends it twice.

import Base
import ./main.bend as Tac

# LAW: min never exceeds its right argument
law min_le:
  for +a: Nat
  for +b: Nat
  {Nat.is_le(Tac.min(a, b), b) == True{} : Bool}

# LAW: a proved order survives a bump on the right
law le_succ_r:
  for  x: Nat
  for +q: Nat
  for px: {Nat.is_le(x, q) == True{} : Bool}
  {Nat.is_le(x, 1n+q) == True{} : Bool}
