# a Base-free module with its own Nat: its literals are THIS Nat's
# constructors, alone and imported alike. Before, a literal unfolded into
# bare Zero and Succ, so the entry's constructors answered for them
type Nat is Data:
  Zero{}
  Succ{p: Nat}

def two() -> Nat:
  2n

def five() -> Nat:
  3n+two()

def main() -> Nat:
  five()

#|5n
