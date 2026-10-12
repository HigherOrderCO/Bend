# The operation itself owns the last handle until its deadline fires.
import Base

def show(result: Poll<&1, &1, Maybe<&1, String>, Unit>) -> String:
  match result:
    case Wait{unit}:
      "deadline"
    case Ready{value}:
      "unexpected ready"

def main() -> IO(Unit):
  do IO<Unit>:
    c : Chan(String) <- Chan.new(String, 0)
    result : Poll<&1, &1, Maybe<&1, String>, Unit> <- Chan.try_recv(String, c, 1)
    IO.print(show(result))

#|deadline
