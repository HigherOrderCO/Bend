# A nested IO that failed takes its parked receiver with it: the outer send
# neither feeds that receiver nor loses its value, and the late child runs.
import Base

def Host.inner(c: Chan(U32)) -> IO(Unit):
  import "./reenter_channel_done.js"

def show(x: Maybe<&1, U32>) -> String:
  match x:
    case Some{value}:
      "outer got " ++ U32.show(value)
    case None{}:
      "closed"

def late() -> IO(Unit):
  do IO<Unit>:
    Unit <- IO.sleep(20)
    IO.print("late")

def go(c: Chan(U32)) -> IO(Unit):
  +ch = c
  do IO<Unit>:
    Unit <- IO.spawn(Unit, late())
    Unit <- Host.inner(ch)
    r : Result<&1, &1, U32, Unit> <- Chan.send(U32, ch, 7)
    x : Maybe<&1, U32> <- Chan.recv(U32, ch)
    IO.print(show(x))

def main() -> IO(Unit):
  IO.bind(Chan(U32), Unit, Chan.new(U32, 1), c => go(c))

#|outer got 7
#|late
