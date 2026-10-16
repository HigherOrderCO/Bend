import Base

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print(Bool.show(F32.is_eq(F32.lerp(3.0e38, F32.neg(3.0e38), 0.5), 0.0)))
    IO.print(Bool.show(F32.is_eq(F32.lerp(1.0e20, 1.0, 1.0), 1.0)))
    IO.print(U32.show(F32.bits(F32.lerp(F32.neg(0.0), 1.0, 0.0))))
    IO.print(Bool.show(F32.is_eq(F32.lerp(1.7014118346046923e38, F32.neg(1.7014118346046923e38), 0.25), 8.507059173023462e37)))

#|True
#|True
#|2147483648
#|True
