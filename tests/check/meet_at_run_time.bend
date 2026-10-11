# a meet in live code goes to --verdict as the kernel's (a <&> b), whose
# live check reads both sides: m is a meet of two run-time quantities,
# main's closed meet folds to &1, and g's kind names m(a, b) where f's
# names a <&> b, which the kernel converts through m. The laws check
# every row of m's table in the emitted book
import Base

def keep(q: Quant, -A: Kind(q), x: A) -> A:
  x

def m(a: Quant, b: Quant) -> Quant:
  a <&> b

def f(a: Quant, b: Quant, -A: Kind(a <&> b), x: A) -> A:
  x

def g(a: Quant, b: Quant, -A: Kind(m(a, b)), x: A) -> A:
  f(a, b, A, x)

law table:
  {m(&0, &0) == &0 : Quant} & {m(&0, &1) == &0 : Quant} & {m(&0, &2) == &0 : Quant}
  & {m(&1, &0) == &0 : Quant} & {m(&1, &1) == &1 : Quant} & {m(&1, &2) == &1 : Quant}
  & {m(&2, &0) == &0 : Quant} & {m(&2, &1) == &1 : Quant} & {m(&2, &2) == &2 : Quant}

def table():
  ({==}, {==}, {==}, {==}, {==}, {==}, {==}, {==}, {==})

def main() -> Nat:
  keep(m(&1, &2), Nat, keep(&2 <&> &1, Nat, g(&2, &2, Nat, 3n)))

#|3n
