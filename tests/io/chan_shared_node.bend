# A channel is Data, so a list cell holding one may be shared: matching
# a shared cell copies the handle out of it rather than failing.
import Base

def show(m: Maybe<&1, U32>) -> String:
  match m:
    case Some{v}:
      U32.show(v)
    case None{}:
      "end"

def send(xs: List<&2, Chan(U32)>, n: U32) -> IO(Unit):
  match xs:
    case []:
      IO.pure(Unit, Unit{})
    case c <> rest:
      do IO<Unit>:
        Result<&1, &1, U32, Unit> <- Chan.send(U32, c, n)
        IO.pure(Unit, Unit{})

def drain(xs: List<&2, Chan(U32)>) -> IO(Unit):
  match xs:
    case []:
      IO.print("empty")
    case c <> rest:
      +h = c
      do IO<Unit>:
        a : Maybe<&1, U32> <- Chan.recv(U32, h)
        Unit <- Chan.close(U32, h)
        b : Maybe<&1, U32> <- Chan.recv(U32, h)
        z : Maybe<&1, U32> <- Chan.recv(U32, h)
        IO.print(show(a) ++ show(b) ++ show(z))

def main() -> IO(Unit):
  do IO<Unit>:
    c : Chan(U32) <- Chan.new(U32, 2)
    +xs : List<&2, Chan(U32)> <- IO.pure(List<&2, Chan(U32)>, [c])
    Unit <- send(xs, 7)
    Unit <- send(xs, 8)
    drain(xs)

#|78end
