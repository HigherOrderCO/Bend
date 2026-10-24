# a do-block bind without its type is a bind, not the exec form with the
# name as the type: say so, instead of "a defined name" for x (#1787)
import Base

def main() -> IO(Unit):
  do IO<Unit>:
    x <- IO.pure(U32, 1)
    IO.print(U32.show(x))

#|SOME PROOFS FAIL
#|Error:
#|- expected : a typed bind (x : A <- m) or let (x : A = v)
#|- observed : 'x <- IO.pure(U32, 1)'
#|Location:
#|6 |   do IO<Unit>:
#|7>|     x <- IO.pure(U32, 1)
#|  |     ^^^^^^^^^^^^^^^^^^^^
#|8 |     IO.print(U32.show(x))
#|exit 1
