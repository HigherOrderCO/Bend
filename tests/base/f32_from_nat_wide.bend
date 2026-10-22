import Base

def cast(n: Nat) -> F32:
  F32.from_nat(n)

def check(+n: Nat, +want: F32) -> IO(Unit):
  do IO<Unit>:
    IO.print(Bool.show(F32.is_eq(F32.from_nat(n), want)))
    IO.print(Bool.show(F32.is_eq(cast!(n), want)))

def main() -> IO(Unit):
  do IO<Unit>:
    check(16777217n, 16777216.0)
    check(Nat.double(2147483648n), 4294967296.0)
    check(Nat.add(Nat.double(2147483648n), 257n), 4294967808.0)
    check(Nat.add(Nat.double(2147483648n), 768n), 4294968320.0)
    check(Nat.add(Nat.mul(65535n, Nat.double(2147483648n)), 4294967295n), 281474976710656.0)

#|True
#|True
#|True
#|True
#|True
#|True
#|True
#|True
#|True
#|True
