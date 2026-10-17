import Base

@unsafe
def Rp.BadType() -> Type:
  Nat

def Rp.dependent(next: @+n: Nat -> Rp.BadType()) -> Nat:
  0n

def main() -> Nat:
  0n
