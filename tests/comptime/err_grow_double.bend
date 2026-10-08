# the growing self-call, doubling: the argument doubles per level, so it
# passes the size a ~ argument may have before it reaches the 64th
# instance, and the call is refused where it crosses
import Base

def inc(n: Nat) -> Nat:
  1n+n

def grow(~f: Nat -> Nat, x: Nat) -> Nat:
  match x:
    case 0n:
      f(0n)
    case 1n+p:
      grow(~(y => f(f(y))), p)

def main() -> Nat:
  grow(~inc, 2n)

#|SOME PROOFS FAIL
#|Error:
#|- expected : a ~ argument that stops growing
#|- observed : grow
#|Context:
#|- p : Nat
#|Location: grow~inst
#|13 |     case 1n+p:
#|14>|       grow(~(y => f(f(y))), p)
#|   |       ^^^^
#|15 |
#|exit 1
