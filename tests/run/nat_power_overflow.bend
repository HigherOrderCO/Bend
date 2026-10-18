# Repeated squaring must check the 48-bit Nat wall, not just u64 overflow.
import Base

def main() -> IO(Unit):
  IO.print(Nat.show(Nat.pow(2n, 48n)))

#|bend: a Nat past the largest immediate 2^48-1
#|exit 1