# Closing preserves copied handles and FIFO values across dequeue/refill.
import Base

def show(value: Maybe<&1, String>) -> String:
  match value:
    case Some{text}:
      text
    case None{}:
      "end"

def main() -> IO(Unit):
  do IO<Unit>:
    +c : Chan(String) <- Chan.new(String, 2)
    a : Result<&1, &1, String, Unit> <- Chan.send(String, c, "a" ++ U32.show(1))
    b : Result<&1, &1, String, Unit> <- Chan.send(String, c, "b" ++ U32.show(2))
    first : Maybe<&1, String> <- Chan.recv(String, c)
    d : Result<&1, &1, String, Unit> <- Chan.send(String, c, "c" ++ U32.show(3))
    Unit <- Chan.close(String, c)
    second : Maybe<&1, String> <- Chan.recv(String, c)
    third : Maybe<&1, String> <- Chan.recv(String, c)
    ended : Maybe<&1, String> <- Chan.recv(String, c)
    Unit <- Chan.close(String, c)
    Unit <- IO.print(show(first))
    Unit <- IO.print(show(second))
    Unit <- IO.print(show(third))
    IO.print(show(ended))

#|a1
#|b2
#|c3
#|end
