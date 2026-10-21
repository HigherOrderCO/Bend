# the same bind on a name that exists: it is not a U32 used as a Type (#1787)
import Base

def f(n: U32) -> IO(Unit):
  do IO<Unit>:
    n <- IO.pure(U32, 1)
    IO.print(U32.show(n))

def main() -> IO(Unit):
  f(2)

#|SOME PROOFS FAIL
#|Error:
#|- expected : a typed bind (x : A <- m) or let (x : A = v)
#|- observed : 'n <- IO.pure(U32, 1)'
#|Location:
#|5 |   do IO<Unit>:
#|6>|     n <- IO.pure(U32, 1)
#|  |     ^^^^^^^^^^^^^^^^^^^^
#|7 |     IO.print(U32.show(n))
#|exit 1
