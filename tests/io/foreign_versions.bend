# Imported versions own distinct host functions and function-local state.
import Base
import ./foreign-versions/one.bend as A
import ./foreign-versions/two.bend as B

def first(value: A.Instant) -> String:
  match value:
    case A.Instant{value}:
      U32.show(value)

def second(value: B.Instant) -> String:
  match value:
    case B.Instant{value}:
      U32.show(value)

def main() -> IO(Unit):
  do IO<Unit>:
    a : A.Instant <- A.now()
    b : B.Instant <- B.now()
    c : A.Instant <- A.now()
    d : B.Instant <- B.now()
    Unit <- IO.print(first(a))
    Unit <- IO.print(second(b))
    Unit <- IO.print(first(c))
    IO.print(second(d))

#|8
#|71
#|9
#|72
