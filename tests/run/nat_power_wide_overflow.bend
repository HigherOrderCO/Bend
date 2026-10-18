# Squaring the largest immediate must fail before the product wraps u64.
import Base

def main() -> IO(Unit):
  largest = Nat.add(Nat.mul(16777215n, 16777216n), 16777215n)
  IO.print(Nat.show(Nat.pow(largest, 2n)))

#|bend: a Nat past the largest immediate 2^48-1
#|exit 1