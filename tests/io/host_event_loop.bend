# JS host waits must release the event loop: microtasks, fulfilled and
# rejected promises, and a timer writing a real native pipe all resume IO.
import Base

def Host.microtask_seen() -> IO(U32):
  import "./host_event_loop.js"

def Host.settled() -> IO(U32):
  import "./host_event_loop.js"

def Host.rejected() -> IO(U32):
  import "./host_event_loop.js"

def Host.descriptor() -> IO(U32):
  import "./host_event_loop.js"

# A fulfilled Promise must interrupt an already-warm native descriptor wait;
# the other computation must still receive the later pipe byte exactly once.
def send_pipe(+c: Chan(U32)) -> IO(Unit):
  do IO<Unit>:
    value : U32 <- Host.descriptor()
    Result<&1, &1, U32, Unit> <- Chan.send(U32, c, value)
    return Unit{}

def show_received(got: Maybe<&1, U32>) -> IO(Unit):
  match got:
    case None{}:
      IO.die(Unit, 1, "missing pipe value")
    case Some{value}:
      IO.print(U32.show(value))

def race(c: Chan(U32)) -> IO(Unit):
  +ch = c
  do IO<Unit>:
    Unit <- IO.spawn(Unit, send_pipe(ch))
    value : U32 <- Host.settled()
    Unit <- IO.print(U32.show(value))
    got : Maybe<&1, U32> <- Chan.recv(U32, ch)
    show_received(got)

def main() -> IO(Unit):
  do IO<Unit>:
    micro : U32 <- Host.microtask_seen()
    Unit <- IO.print(U32.show(micro))
    settled : U32 <- Host.settled()
    Unit <- IO.print(U32.show(settled))
    rejected : U32 <- Host.rejected()
    Unit <- IO.print(U32.show(rejected))
    descriptor : U32 <- Host.descriptor()
    Unit <- IO.print(U32.show(descriptor))
    c : Chan(U32) <- Chan.new(U32, 1)
    race(c)

#|1
#|42
#|71
#|43
#|42
#|43
