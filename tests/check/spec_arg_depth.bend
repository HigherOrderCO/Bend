# a specialized argument goes out where it is used, at that depth and
# liveness: in k, f is specialized (A's kind names it), so its argument
# (y => y) is spliced into k's body. Elaborated where it was bound, its
# binder took g's level, and carrying g into the match's arms renamed the
# identity's body to g; and elaborated dead, the root's constant for f
# went out after the def that calls it (#1168). Elaborated where bound
# and bare, a λ-match with a default arm was out of scope, even when only
# a kind names it (k3). The law checks main's value in the emitted book
# too, where a wrong but well-typed body would still pass (see
# spec_arg_unused_column.bend)
import Base

def qof2(f: Nat -> Nat) -> Quant:
  &1

def ap(f: Nat -> Nat, x: Nat) -> Nat:
  f(x)

def k(f: Nat -> Nat, g: Nat -> Nat, -A: Kind(qof2(f)), b: Bool) -> Nat:
  match b:
    case True{}:  f(g(0n))
    case False{}: f(g(1n))

def k2(f: Nat -> Nat, +g: Nat, -A: Kind(qof2(f)), b: Bool) -> Nat:
  match b:
    case True{}:  Nat.add(ap(f, 5n), g)
    case False{}: g

def qb(f: Bool -> Nat) -> Quant:
  &1

def k3(f: Bool -> Nat, -A: Kind(qb(f)), n: Nat) -> Nat:
  n

def k4(f: Bool -> Nat, -A: Kind(qb(f)), b: Bool) -> Nat:
  f(b)

def main() -> Nat:
  Nat.add(Nat.add(k((y => y), (z => z), Unit, True{}), k2((y => 1n+y), 0n, Unit, True{})),
    Nat.add(k3(\{True: 1n; b => 0n}, Unit, 3n), k4(\{True: 1n; b => 0n}, Unit, True{})))

law spec_arg_depth_result:
  {main() == 10n : Nat}

def spec_arg_depth_result():
  {==}

#|10n
