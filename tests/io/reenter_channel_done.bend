# A nested IO that failed takes its parked waiter with it: a dead sender's
# value is dropped, a dead receiver (timed or not) takes nothing, and close
# skips them.
import Base

def Host.inner(c: Chan(U32), mode: U32) -> IO(Unit):
  import "./reenter_channel_done.js"

def show(x: Maybe<&1, U32>) -> String:
  match x:
    case Some{value}:
      "got " ++ U32.show(value)
    case None{}:
      "closed"

def closer(c: Chan(U32)) -> IO(Unit):
  do IO<Unit>:
    Unit <- IO.sleep(10)
    Chan.close(U32, c)

def dead_sender(c: Chan(U32)) -> IO(Unit):
  +ch = c
  do IO<Unit>:
    Unit <- Host.inner(ch, 0)
    Unit <- IO.spawn(Unit, closer(ch))
    x : Maybe<&1, U32> <- Chan.recv(U32, ch)
    IO.print("sender " ++ show(x))

def dead_receiver(c: Chan(U32), +mode: U32) -> IO(Unit):
  +ch = c
  do IO<Unit>:
    Unit <- Host.inner(ch, mode)
    Unit <- IO.sleep(10)
    r : Result<&1, &1, U32, Unit> <- Chan.send(U32, ch, 7)
    x : Maybe<&1, U32> <- Chan.recv(U32, ch)
    IO.print("receiver " ++ U32.show(mode) ++ " " ++ show(x))

def dead_closed(c: Chan(U32)) -> IO(Unit):
  +ch = c
  do IO<Unit>:
    Unit <- Host.inner(ch, 2)
    Unit <- Host.inner(ch, 2)
    Chan.close(U32, ch)

def main() -> IO(Unit):
  do IO<Unit>:
    Unit <- IO.bind(Chan(U32), Unit, Chan.new(U32, 0), c => dead_sender(c))
    Unit <- IO.bind(Chan(U32), Unit, Chan.new(U32, 1), c => dead_receiver(c, 1))
    Unit <- IO.bind(Chan(U32), Unit, Chan.new(U32, 1), c => dead_receiver(c, 2))
    Unit <- IO.bind(Chan(U32), Unit, Chan.new(U32, 0), c => dead_closed(c))
    IO.print("end")

#|sender closed
#|receiver 1 got 7
#|receiver 2 got 7
#|end
