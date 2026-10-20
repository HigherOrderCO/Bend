# A receiver parked by a nested IO that has since failed still gets the value:
# the running IO adopts it, and counts it, so the late child still runs.
import Base

def Host.inner(c: Chan(U32)) -> IO(Unit):
  import "./reenter_channel_done.js"

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
    IO.print("outer sent")

def main() -> IO(Unit):
  IO.bind(Chan(U32), Unit, Chan.new(U32, 0), c => go(c))

#|outer sent
#|inner fiber got 7
#|late
