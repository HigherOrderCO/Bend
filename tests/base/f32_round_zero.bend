import Base

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print(U32.show(F32.bits(F32.round(F32.neg(0.0)))))
    IO.print(Bool.show(F32.is_eq(F32.div(1.0, F32.round(F32.neg(0.1))), F32.div(F32.neg(1.0), 0.0))))
    IO.print(U32.show(F32.bits(F32.round(F32.neg(0.5)))))
    IO.print(Bool.show(F32.is_eq(F32.round(F32.neg(0.5000000596046448)), F32.neg(1.0))))

#|2147483648
#|True
#|2147483648
#|True
