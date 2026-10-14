# a lazy accumulator deeper than the host stack must still evaluate:
# each go step binds acc to a thunk over Nat.add(acc, 1n), so the head
# demand at the end collapses a 262144-long thunk chain -- the head
# Succ can only surface after every link resolves. Its main is IO, so
# `bend` runs it through Comp.io_run on the emitted JS: this is an IO
# and backend case, holding the compiled lanes, strict in their
# arguments, to the answer. The source interpreter never forces the
# chain here; deep_thunk_chain_pure, with a pure Bool main, pins that.
import Base

law Nat.pow2:
  for n: Nat
  Nat

def Nat.pow2(n):
  match n:
    case 0n:
      1n
    case 1n+p:
      Nat.double(Nat.pow2(p))

law go:
  for n   : Nat
  for acc : Nat
  Nat

def go(n, acc):
  match n:
    case 0n:
      acc
    case 1n+p:
      go(p, Nat.add(acc, 1n))

law main.go:
  for n: Nat
  IO(Unit)

def main.go(n):
  match n:
    case 0n:
      IO.print("zero")
    case 1n+p:
      IO.print("big")

law main:
  IO(Unit)

def main():
  main.go(go(Nat.pow2(18n), 0n))

#|big
