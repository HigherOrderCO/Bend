import Base

def Rp.ok(x: Nat) -> Nat:
  Nat.double(x)

def main() -> Nat:
  Rp.ok(2n)
