# a kind goes to --verdict as the kernel's *(q), and a meet as (a <&> b),
# so a kind may depend on any value: a matched parameter (run3, #1175), a
# list read by recursion (elem_q), a quantity picked at run time (pick),
# and a datatype's kind may be a meet (Duo). Each def goes out once, as
# written, and a branch may rely on its kind (dup copies x only where A
# is Data), and a meet reduces next to a stuck side (x <&> &2 is x). A
# family of Data types fits where one of Types is expected, and a function
# over any type where one over Data types is: they go out eta-long, so the
# kernel fits them at the leaves (fam, poly). The law checks main's value
# in the emitted book
import Base

type Box is Type:
  Mk{fn: Nat -> Nat}

def q1(f: Box) -> Quant:
  match f:
    case Mk{g}: &1

def run3(f: Box, -A: Kind(q1(f)), +x: Nat) -> Nat:
  match f:
    case Mk{g}:
      match x:
        case 0n: g(x)
        case 1n+p: g(p)

type Mode is Data:
  Shared{}
  Owned{}

def elem_q(stages: List<&2, Mode>) -> Quant:
  match stages:
    case Nil{}: &1
    case Con{Shared{}, rest}: &2
    case Con{Owned{}, rest}: elem_q(rest)

def run(stages: List<&2, Mode>, -A: Kind(elem_q(stages)), x: A) -> A:
  x

def pick(b: Bool) -> Quant:
  match b:
    case True{}: &2
    case False{}: &1

def keep(q: Quant, -A: Kind(q), x: A) -> A:
  x

def meet(a: Quant, b: Quant, -A: Kind(a <&> b), x: A) -> A:
  x

def use(+b: Bool, x: Nat) -> Nat:
  meet(pick(b), pick(False{}), Nat, keep(pick(b), Nat, x))

type Duo<a, b, -A: Kind(a), -B: Kind(b)> is Kind(a <&> b):
  MkDuo{x: A, y: B}

def first(-a: Quant, -b: Quant, -A: Kind(a), -B: Kind(b), p: Duo<a, b, A, B>) -> A:
  match p:
    case MkDuo{x, y}: x

def mode_q(m: Mode) -> Quant:
  match m:
    case Shared{}: &2
    case Owned{}: &1

def dup(m: Mode, -A: Kind(mode_q(m)), x: A, d: A) -> (A & A):
  match m:
    case Shared{}:
      +y = x
      (y, y)
    case Owned{}:
      (x, d)

def same_kind(-x: Quant) -> {Kind(x <&> &2) == Kind(x) : Type}:
  {==}

def F(n: Nat) -> Data:
  Nat

def at0(-G: Nat -> Type, x: G(0n)) -> G(0n):
  x

def fam() -> Nat:
  at0(F, 3n)

def pick_any(-A: Type, x: A) -> A:
  x

def twice(f: @-A: Data -> A -> A) -> Nat:
  f(Nat, 1n)

def poly() -> Nat:
  twice(pick_any)

def add(p: Nat & Nat) -> Nat:
  match p:
    case (a, b): Nat.add(a, b)

def main() -> Nat:
  Nat.add(Nat.add(Nat.add(run3(Mk{(y: Nat) => 1n}, Nat, 0n), run(Con{Owned{}, Con{Shared{}, Nil{}}}, Nat, 2n)),
    Nat.add(use(True{}, 3n), first(&2, &1, Nat, Nat -> Nat, MkDuo{4n, (z: Nat) => z}))),
    Nat.add(add(dup(Shared{}, Nat, 5n, 9n)), Nat.add(Nat.add(fam(), poly()), 2n)))

law main_is_26:
  {main() == 26n : Nat}

def main_is_26():
  {==}

#|26n
