# f is specialized (A's kind names it), so its argument (y => y) is
# spliced into loose's body. Elaborated where it was bound, its binder
# took a level of g and h, which the match carries into its arms, and
# the emitted use computed g's 7n, not h's 3n: a well-typed body that
# --verdict accepted. The law checks use's value in the emitted book (#1178)
import Base

type Box is Type:
  Mk{fn: Nat -> Nat}

def qbox(f: Box -> Box) -> Quant:
  &1

def get(b: Box) -> Nat:
  match b:
    case Mk{fn}: fn(0n)

def loose(f: Box -> Box, g: Box, h: Box, -A: Kind(qbox(f)), b: Bool) -> Nat:
  match b:
    case True{}:  get(f(h))
    case False{}: get(f(h))

def use() -> Nat:
  loose((y => y), Mk{(z => 7n)}, Mk{(w => 3n)}, Unit, True{})

law use_is_3:
  {use() == 3n : Nat}

def use_is_3():
  {==}

def main() -> Nat:
  use()

#|3n
