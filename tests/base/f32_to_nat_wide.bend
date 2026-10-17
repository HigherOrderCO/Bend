import Base

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print(Nat.show(F32.to_nat(42.75)))
    IO.print(Nat.show(F32.to_nat(F32.neg(42.0))))
    IO.print(Nat.show(F32.to_nat(4294967296.0)))
    IO.print(Nat.show(F32.to_nat(4294967808.0)))
    IO.print(Nat.show(F32.to_nat(281474959933440.0)))

#|42
#|0
#|4294967296
#|4294967808
#|281474959933440
