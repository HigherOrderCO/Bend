# an exec statement whose type is a lowercase type variable is still the
# exec form: only a name that cannot be a type reads as a bind that lost
# its type (#1787)
import Base

def f(-a: Type, x: a) -> IO(Unit):
  do IO<Unit>:
    a <- IO.pure(a, x)
    IO.print("ok")

def main() -> IO(Unit):
  f(U32, 1)

#|ok
