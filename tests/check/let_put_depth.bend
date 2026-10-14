# Inlined self-call arguments are built at the use site's depth. A convoy
# must neither capture their bound variables nor move a free variable onto
# an inner binder. The law checks the translated value, not just its type.
import Base

def go(n: Nat, g: (Nat -> Nat) -> Nat) -> Nat:
  match n:
    case Zero{}:
      g(x => 0n)
    case Succ{p}:
      at0 = {(f => f(0n)) : (Nat -> Nat) -> Nat}
      one = {(x => 1n) : Nat -> Nat}
      {\{True: go(p, at0); False: one(0n)} : Bool -> Nat}(True{})

def app2(f: Nat -> (Nat -> Nat) -> Nat) -> Nat:
  f(1n)((z => 50n))

def fc(n: Nat, m: Nat -> Nat, acc: Nat) -> Nat:
  match n:
    case Zero{}:
      acc
    case Succ{p}:
      c = {Succ{app2((u => (w => m(u))))} : Nat}
      {\{True: fc(p, (x => x), c); False: c} : Bool -> Nat}(True{})

def main() -> Nat:
  Nat.add(go(1n, (f => 0n)), fc(1n, (x => 8n), 0n))

law let_put_depth_result:
  {main() == 9n : Nat}

def let_put_depth_result():
  {==}

#|9n
