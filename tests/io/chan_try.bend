# io (io_chan_try): the try_ twins of Chan.send and Chan.recv. With room
# for one, a send goes and the next waits; a receive takes the value and
# the next waits. A receive bounded by 20 ms that nothing answers comes
# back Wait{} after its deadline and leaves the channel, so a later send
# is not swallowed. A sender parked with a deadline hands its value to a
# receiver that comes first. Closing answers a parked receiver None{}
# through Ready, and a send to a closed channel hands its value back.
import Base

def poll_send(r: Poll<&1, &1, Result<&1, &1, U32, Unit>, U32>) -> String:
  match r:
    case Ready{Done{u}}:
      "sent"
    case Ready{Fail{v}}:
      "closed, kept " ++ U32.show(v)
    case Wait{v}:
      "full, kept " ++ U32.show(v)

def poll_recv(r: Poll<&1, &1, Maybe<&1, U32>, Unit>) -> String:
  match r:
    case Ready{Some{v}}:
      "got " ++ U32.show(v)
    case Ready{None{}}:
      "ended"
    case Wait{u}:
      "nothing"

def got(m: Maybe<&1, U32>) -> String:
  match m:
    case Some{v}:
      "got " ++ U32.show(v)
    case None{}:
      "ended"

def sent(r: Result<&1, &1, U32, Unit>) -> String:
  match r:
    case Done{u}:
      "sent"
    case Fail{v}:
      "closed, kept " ++ U32.show(v)

def say(-A: Type, show: A -> String, act: IO(A)) -> IO(Unit):
  IO.bind(A, Unit, act, x => IO.print(show(x)))

def late_send(c: Chan(U32)) -> IO(Unit):
  do IO<Unit>:
    u : Unit <- IO.sleep(60)
    say(Result<&1, &1, U32, Unit>, sent, Chan.send(U32, c, 7))

def roomy(+c: Chan(U32)) -> IO(Unit):
  do IO<Unit>:
    a : Unit <- say(Poll<&1, &1, Result<&1, &1, U32, Unit>, U32>, poll_send, Chan.try_send(U32, c, 1, 0))
    b : Unit <- say(Poll<&1, &1, Result<&1, &1, U32, Unit>, U32>, poll_send, Chan.try_send(U32, c, 2, 0))
    d : Unit <- say(Poll<&1, &1, Maybe<&1, U32>, Unit>, poll_recv, Chan.try_recv(U32, c, 0))
    g : Unit <- say(Poll<&1, &1, Maybe<&1, U32>, Unit>, poll_recv, Chan.try_recv(U32, c, 0))
    h : Unit <- IO.spawn(Unit, late_send(c))
    i : Unit <- say(Poll<&1, &1, Maybe<&1, U32>, Unit>, poll_recv, Chan.try_recv(U32, c, 20))
    say(Maybe<&1, U32>, got, Chan.recv(U32, c))

def closing(+c: Chan(U32)) -> IO(Unit):
  do IO<Unit>:
    h : Unit <- IO.spawn(Unit, IO.bind(Unit, Unit, IO.sleep(20), u => Chan.close(U32, c)))
    i : Unit <- say(Poll<&1, &1, Maybe<&1, U32>, Unit>, poll_recv, Chan.try_recv(U32, c, 5000))
    say(Result<&1, &1, U32, Unit>, sent, Chan.send(U32, c, 9))

def handoff(+c: Chan(U32)) -> IO(Unit):
  do IO<Unit>:
    h : Unit <- IO.spawn(Unit, say(Poll<&1, &1, Result<&1, &1, U32, Unit>, U32>, poll_send, Chan.try_send(U32, c, 5, 5000)))
    u : Unit <- IO.sleep(20)
    say(Maybe<&1, U32>, got, Chan.recv(U32, c))

def main() -> IO(Unit):
  do IO<Unit>:
    a : Chan(U32) <- Chan.new(U32, 1)
    u : Unit <- roomy(a)
    b : Chan(U32) <- Chan.new(U32, 0)
    v : Unit <- closing(b)
    c : Chan(U32) <- Chan.new(U32, 0)
    handoff(c)

#|sent
#|full, kept 2
#|got 1
#|nothing
#|nothing
#|sent
#|got 7
#|ended
#|closed, kept 9
#|got 5
#|sent
