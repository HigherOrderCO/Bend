# a self-call whose compile-time argument grows one node per level never
# reaches a shared instance: each instance's check mints the next, so the
# chain stops at the 64th level, in the instance whose self-call crosses it
import Base

def inc(n: Nat) -> Nat:
  1n+n

def grow(~f: Nat -> Nat, x: Nat) -> Nat:
  match x:
    case 0n:
      f(0n)
    case 1n+p:
      grow(~(y => inc(f(y))), p)

def main() -> Nat:
  grow(~inc, 2n)

#|SOME PROOFS FAIL
#|Error:
#|- expected : a template that stops instantiating itself (64 levels at most)
#|- observed : grow
#|Context:
#|- p : Nat
#|Location: grow~inst
#|13 |     case 1n+p:
#|14>|       grow(~(y => inc(f(y))), p)
#|   |       ^^^^
#|15 |
#|exit 1
