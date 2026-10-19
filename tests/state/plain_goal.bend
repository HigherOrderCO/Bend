# word-switch ladder, state 0: the whole goal stuck on the open var --
# both sides render as REF calls (never an internal match literal)
# ported from bend3: the U32 switch (case 0 / case 1+n) has no bend
# surface, so the ladder counts on Nat (0n / 1n+n)
import Base
type MN is Data:
  Z{}
  S{p: MN}

law ones:
  for n: Nat
  MN

def ones(n):
  match n:
    case 0n:
      Z{}
    case 1n+n:
      S{ones(n)}

law twos:
  for n: Nat
  MN

def twos(n):
  match n:
    case 0n:
      Z{}
    case 1n+n:
      S{twos(n)}

law ones_twos:
  for n: Nat
  {ones(n) == twos(n) : MN}

def ones_twos(n):
  {==}

#|SOME PROOFS FAIL
#|Error:
#|- message  : {==} needs both sides to compute to the same value
#|Context:
#|- n : Nat
#|Location: ones_twos
#|36 | def ones_twos(n):
#|37>|   {==}
#|   |   ^^^^
#|38 |
#|- left     : ones(n)  =>  ones(n)
#|- right    : twos(n)  =>  twos(n)
#|exit 1
