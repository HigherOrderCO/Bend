# io (io_chan_try_deadlock): a try_ waiter woken before its deadline
# drops its timer, so the loop still sees a deadlock at once. A receive
# bounded by 60 s parks, a sender answers it Ready{Some{1}}, and a recv
# that nothing will answer follows: the loop reports the deadlock and
# exits 1 instead of waiting out the 60 s.
import Base

law main:
  IO(U32)

def show(r: Poll<&1, &1, Maybe<&1, U32>, Unit>) -> String:
  match r:
    case Ready{Some{v}}:
      "got " ++ U32.show(v)
    case Ready{None{}}:
      "ended"
    case Wait{u}:
      "nothing"

def wait(+c: Chan(U32)) -> IO(U32):
  do IO<U32>:
    Unit <- IO.spawn(Unit, IO.bind(Result<&1, &1, U32, Unit>, Unit, Chan.send(U32, c, 1), r => IO.pure(Unit, Unit{})))
    r : Poll<&1, &1, Maybe<&1, U32>, Unit> <- Chan.try_recv(U32, c, 60000)
    Unit <- IO.print(show(r))
    got : Maybe<&1, U32> <- Chan.recv(U32, c)
    return 0

def go(c: Chan(U32)) -> IO(U32):
  wait(c)

def main():
  IO.bind(Chan(U32), U32, Chan.new(U32, 0), go)

#|got 1
#|bend: deadlock: every computation waits on a channel
#|exit 1
