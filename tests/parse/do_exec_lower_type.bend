# a declared type with a lowercase name is a type: `foo <- m` is the exec
# form, not a bind that lost its type (#1787)
import Base

type foo is Type:
  Mkfoo{}

def main() -> IO(Unit):
  do IO<Unit>:
    foo <- IO.pure(foo, Mkfoo{})
    IO.print("ok")

#|ok
