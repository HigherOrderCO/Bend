# the entry declares its own Nat; the module's literals still build the
# module's Nat
import ./lit_own_nat_lib.bend as L

type Nat is Data:
  Zero{}
  Succ{p: Nat, q: Nat}

def main() -> L.Nat:
  L.five()

#|lit_own_nat_lib.Succ{lit_own_nat_lib.Succ{lit_own_nat_lib.Succ{lit_own_nat_lib.Succ{lit_own_nat_lib.Succ{lit_own_nat_lib.Zero{}}}}}}
