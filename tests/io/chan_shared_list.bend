# A channel inside a shared Data cell survives both list matches.
import Base

def send(xs: List<&2, Chan(U32)>) -> IO(Unit):
  match xs:
    case Nil{}:
      IO.pure(Unit, Unit{})
    case Con{head, tail}:
      do IO<Unit>:
        result : Result<&1, &1, U32, Unit> <- Chan.send(U32, head, 42)
        send(tail)

def got(value: Maybe<&1, U32>) -> U32:
  match value:
    case None{}:
      0
    case Some{number}:
      number

def receive(xs: List<&2, Chan(U32)>) -> IO(Unit):
  match xs:
    case Nil{}:
      IO.pure(Unit, Unit{})
    case Con{head, tail}:
      +channel = head
      do IO<Unit>:
        value : Maybe<&1, U32> <- Chan.recv(U32, channel)
        Unit <- Chan.close(U32, channel)
        Unit <- IO.print(U32.show(got(value)))
        receive(tail)

def main() -> IO(Unit):
  do IO<Unit>:
    channel : Chan(U32) <- Chan.new(U32, 1)
    +xs : List<&2, Chan(U32)> = Con{channel, Nil{}}
    Unit <- send(xs)
    receive(xs)

#|42
