# io (io_chan_close_send): a send answers Done{} while the channel is
# open and Fail{value} once it is closed, handing the value back, on
# every lane.
import Base

law show:
  for r: Result<&1, &1, U32, Unit>
  String

def show(r):
  match r:
    case Fail{v}:
      String.append("Fail", U32.show(v))
    case Done{u}:
      "Done"

law main.go:
  for c: Chan(U32)
  IO(U32)

def main.go(c):
  +ch = c
  do IO<U32>:
    a : Result<&1, &1, U32, Unit> <- Chan.send(U32, ch, 1)
    Unit <- Chan.close(U32, ch)
    b : Result<&1, &1, U32, Unit> <- Chan.send(U32, ch, 2)
    Unit <- IO.print(String.append(show(a), show(b)))
    return 0

law main:
  IO(U32)

def main():
  IO.bind(Chan(U32), U32, Chan.new(U32, 2), main.go)

#|DoneFail2
