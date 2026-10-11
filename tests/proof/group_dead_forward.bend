# Only live calls form groups. h first names unrelated L2 in a dead
# argument, then L; neither reference makes h or d a member of L.
# The laws catch a dead reference rerouting d through L's selector.
import Base

def keep(-p: Nat, x: Nat) -> Nat:
  x

law L:
  for n: Nat
  Nat

law L2:
  for n: Nat
  Nat

def h(n: Nat) -> Nat:
  keep(L2(n), keep(L(n), n))

def d(n: Nat) -> Nat:
  keep(L(n), 1n+n)

def L(n):
  match n:
    case Zero{}:
      0n
    case Succ{m}:
      h(m)

def L2(n):
  0n

law helper:
  {L(4n) == 3n : Nat}

def helper():
  {==}

law non_member:
  {d(4n) == 5n : Nat}

def non_member():
  {==}

def main() -> Nat:
  d(4n)

#|5n
