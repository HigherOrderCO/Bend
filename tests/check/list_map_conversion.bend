# A closed template callback reduces inside equality types and at runtime.
import Base

law LL.length:
  for -A: Type
  for xs: List<A>
  Nat

def LL.length(A, xs):
  match xs:
    case Nil{}:
      0n
    case h <> t:
      (1n + LL.length(A, t) : Nat)

law lmap:
  for ~f: Nat -> Nat
  for xs: List<Nat>
  List<Nat>

def lmap(f, xs):
  match xs:
    case Nil{}:
      Nil{}
    case h <> t:
      f(h) <> lmap(~f, t)

def mapped_length()
  -> {LL.length(Nat, lmap(~(x => (x + 1n : Nat)), [1n, 2n])) == 2n : Nat}:
  {==}

def mapped_values()
  -> {lmap(~(x => (x + 1n : Nat)), [1n, 2n]) == [2n, 3n] : List<Nat>}:
  {==}

def sum(xs: List<Nat>) -> Nat:
  match xs:
    case Nil{}:
      0n
    case h <> t:
      (h + sum(t) : Nat)

def main() -> Nat:
  sum(lmap(~(x => (x + 1n : Nat)), [1n, 2n]))

#|5n
