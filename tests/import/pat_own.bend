# the entry declares its own Nat and List; the module's patterns still match
# the module's constructors
import ./pat_own_lib.bend as L

type Nat is Data:
  Zero{}
  Succ{p: Nat, q: Nat}

type List is Data:
  Nil{}

def main() -> L.Nat:
  L.run()

#|pat_own_lib.Succ{pat_own_lib.Succ{pat_own_lib.Zero{}}}
