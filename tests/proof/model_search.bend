# recursive template models must terminate without missing a later leaf.
# dependent lambda bodies are rebound after their bounded search finishes.
import Base

type W<-F: Nat -> Type> is Type:
  C{g: Nat -> W<F>}

def ignore_w(~w: W<(n => Nat)>, y: Nat) -> Nat:
  y

type Nest<-A: Type> is Type:
  NCons{x: A, rest: Nest<Pair(A, A)>}
  NNil{}

def ignore_n(~n: Nest<Nat>, y: Nat) -> Nat:
  y

def use_proof(~p: @a: Nat -> @b: Nat -> {a == a : Nat}, a: Nat, b: Nat) -> {a == a : Nat}:
  p(a, b)

def twice(~f: Nat -> Nat, n: Nat) -> Nat:
  f(f(n))

def main() -> Nat:
  ignore_n(NNil{}, twice(n => 1n+n, 5n))

#|7n
