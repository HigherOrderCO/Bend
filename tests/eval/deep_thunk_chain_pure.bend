# the source interpreter must collapse a lazy accumulator deeper than
# the host stack: deep_thunk_chain pins this 262144-link Nat chain, but
# its IO main runs through the JS lane, so the interpreter never forces
# the chain that pin describes. This twin keeps go and the match-walk
# head demand and answers a Bool, so `bend deep_thunk_chain_pure.bend`
# evaluates it lazily in term_wnf: each go step binds acc to a thunk
# over Nat.add(acc, 1n), and the match on the result can only pick its
# arm after every link resolves. The compiled lanes, strict in their
# arguments, hold the same answer.
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

law main.big:
  for n: Nat
  Bool

def main.big(n):
  match n:
    case 0n:
      False{}
    case 1n+p:
      True{}

law main:
  Bool

def main():
  main.big(go(Nat.pow2(18n), 0n))

#|True{}
