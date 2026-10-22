# a proof error whose fields hold a computed 3000-deep chain must
# still print its error page (depth unbounded by source never kills a
# page). A level is one term_check under tele_check; 6000 of them sat
# within 5% of the 8 MB main-thread stack (a one-shard --checkup died
# at 5731 once the FTL tier's frames grew, on x86-64 and arm64 alike);
# 3000 keeps 2x headroom.
# ported from bend3: U32 index -> Nat; headless chain (a U32 head
# prints as a 32-deep Word chain, bloating the golden without adding
# depth)
import Base
type L is Data:
  LC{tail: L}
  LN{}

law build:
  for n: Nat
  L

def build(n):
  match n:
    case 0n:
      LN{}
    case 1n+n:
      LC{build(n)}

law thm:
  {build(3000n) == LN{} : L}

def thm():
  {==}

#|SOME PROOFS FAIL
#|Error:
#|- message  : {==} needs both sides to compute to the same value
#|Location: thm
#|29 | def thm():
#|30>|   {==}
#|   |   ^^^^
#|31 |
#|- left     : build(3000n)  =>  LC{build(2999n)}
#|- right    : LN{}  =>  LN{}
#|exit 1
