# An exported def takes a Nat as a whole number from 0 to 2^53: a
# negative, fractional, BigInt, NaN, infinite or larger argument throws
# the host-range error, in one call or curried, and 0, 2^48 and 2^53
# still round-trip.
import Base

def Nat.host_export() -> IO(U32):
  import "./nat_host_export.js"

def main() -> IO(Unit):
  do IO<Unit>:
    value : U32 <- Nat.host_export()
    IO.print(U32.show(value))

#|0
