# io (io_channel_fork_join): bend3's channel round trip on bend's
# surface. A spawned producer sends 11 and 22 into a channel with
# room for four and closes it; main receives both in order (Some) and
# prints 1122, then forks a pure value and joins it. main's value is
# not the exit status: the process exits 0 once every computation
# has answered.
import Base

law producer:
  for +c: Chan(U32)
  IO(Unit)

def producer(c):
  do IO<Unit>:
    a : Result<&1, &1, U32, Unit> <- Chan.send(U32, c, 11)
    b : Result<&1, &1, U32, Unit> <- Chan.send(U32, c, 22)
    Chan.close(U32, c)

law got:
  for m: Maybe<&1, U32>
  U32

def got(m):
  match m:
    case None{}:
      0
    case Some{value}:
      value

law main.go:
  for c: Chan(U32)
  IO(U32)

def main.go(c):
  +ch = c
  do IO<U32>:
    u : Unit <- IO.spawn(Unit, producer(ch))
    x : Maybe<&1, U32> <- Chan.recv(U32, ch)
    y : Maybe<&1, U32> <- Chan.recv(U32, ch)
    z : Unit <- IO.print(U32.show((got(x) * 100 + got(y) : U32)))
    t : Chan(U32) <- IO.fork(U32, IO.pure(U32, 5))
    w : U32 <- IO.join(U32, t)
    return w

law main:
  IO(U32)

def main():
  IO.bind(Chan(U32), U32, Chan.new(U32, 4), main.go)

#|1122
