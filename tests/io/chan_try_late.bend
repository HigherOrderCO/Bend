# io (io_chan_try_late): expired channel waiters are withdrawn before any
# computation resumes. Register the sender's 1 ms timer before the receive's
# 10 ms deadline, then hold the loop with synchronous clock reads for 20 ms.
# Both timers are overdue: try_recv answers Wait, and the next recv takes 7.
import Base

def show(r: Poll<&1, &1, Maybe<&1, U32>, Unit>) -> String:
  match r:
    case Ready{Some{v}}:
      "A: ready got " ++ U32.show(v)
    case Ready{None{}}:
      "A: ready ended"
    case Wait{u}:
      "A: wait"

def got(m: Maybe<&1, U32>) -> String:
  match m:
    case Some{v}:
      "A: then got " ++ U32.show(v)
    case None{}:
      "A: then ended"

def busy.next(early: Bool, next: IO(Unit)) -> IO(Unit):
  match early:
    case True{}:
      next
    case False{}:
      IO.print("B: held")

# Fuel makes the clock loop terminating; exhaustion fails the test.
def busy.loop(fuel: Nat, +until: Nat) -> IO(Unit):
  match fuel:
    case 0n:
      IO.die(Unit, 1, "test clock did not advance")
    case 1n+p:
      do IO<Unit>:
        now : Nat <- IO.now()
        busy.next(Nat.is_lt(now, until), busy.loop(p, until))

def busy() -> IO(Unit):
  do IO<Unit>:
    start : Nat <- IO.now()
    busy.loop(4294967295n, Nat.add(start, 20n))

def recv(+c: Chan(U32)) -> IO(Unit):
  do IO<Unit>:
    Unit <- IO.spawn(Unit, busy())
    r : Poll<&1, &1, Maybe<&1, U32>, Unit> <- Chan.try_recv(U32, c, 10)
    Unit <- IO.print(show(r))
    m : Maybe<&1, U32> <- Chan.recv(U32, c)
    IO.print(got(m))

def send(+c: Chan(U32)) -> IO(Unit):
  do IO<Unit>:
    Unit <- IO.spawn(Unit, recv(c))
    Unit <- IO.sleep(1)
    Result<&1, &1, U32, Unit> <- Chan.send(U32, c, 7)
    IO.print("S: sent")

def main() -> IO(Unit):
  IO.bind(Chan(U32), Unit, Chan.new(U32, 0), c => send(c))

#|B: held
#|A: wait
#|A: then got 7
#|S: sent
