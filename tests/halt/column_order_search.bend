# bend3 SEARCHED column orders and accepted the swapped Ackermann;
# bend's descent law is fixed left-to-right and its match laws bind
# tighter still -- the swapped spelling dies at the nested match's
# binder order, before any search could exist
import Base

law ack:
  for n: Nat
  for m: Nat
  Nat

def ack(n, m):
  match m:
    case Zero{}:
      Succ{n}
    case Succ{p}:
      match n:
        case Zero{}:
          ack(1n, p)
        case Succ{q}:
          ack(ack(q, Succ{p}), p)

law main:
  Nat

def main():
  ack(3n, 2n)

#|SOME PROOFS FAIL
#|Error:
#|- message  : 'n' can't be matched in this position (it is matched after a local statement or after a match on a later binder, it was already matched, or it is a def)
#|Location:
#|16 |     case Succ{p}:
#|17>|       match n:
#|   |             ^
#|18 |         case Zero{}:
#|exit 1
