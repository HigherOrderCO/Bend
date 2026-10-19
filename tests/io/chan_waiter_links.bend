# Channel waiters can leave by timeout, wake, or close. Each worker signals
# a buffered ready channel immediately before it parks. The parent waits
# for that signal before it adds the next worker or changes the queue.
import Base

def check_eq(ok: Bool) -> IO(Unit):
  match ok:
    case True{}:
      IO.pure(Unit, Unit{})
    case False{}:
      IO.die(Unit, 1, "unexpected channel result")

def check(got: U32, want: U32) -> IO(Unit):
  check_eq(U32.is_eq(got, want))

def kept_eq(ok: Bool, code: U32) -> U32:
  match ok:
    case True{}:
      code
    case False{}:
      999

def kept(got: U32, want: U32, code: U32) -> U32:
  kept_eq(U32.is_eq(got, want), code)

def sent(n: U32, result: Result<&1, &1, U32, Unit>) -> U32:
  match result:
    case Done{unit}:
      n
    case Fail{value}:
      kept(value, n, 100)

def sent_poll(n: U32, result: Poll<&1, &1, Result<&1, &1, U32, Unit>, U32>) -> U32:
  match result:
    case Ready{value}:
      sent(n, value)
    case Wait{value}:
      kept(value, n, 200)

def received(result: Maybe<&1, U32>) -> U32:
  match result:
    case Some{value}:
      value
    case None{}:
      100

def received_poll(result: Poll<&1, &1, Maybe<&1, U32>, Unit>) -> U32:
  match result:
    case Ready{value}:
      received(value)
    case Wait{unit}:
      200

# ms = 0 selects a blocking operation; a positive ms selects a timeout.
def action_go(c: Chan(U32), senders: Bool, +n: U32, ms: U32, blocking: Bool) -> IO(U32):
  match senders blocking:
    case True{} True{}:
      IO.bind(Result<&1, &1, U32, Unit>, U32, Chan.send(U32, c, n), r => IO.pure(U32, sent(n, r)))
    case True{} False{}:
      IO.bind(Poll<&1, &1, Result<&1, &1, U32, Unit>, U32>, U32, Chan.try_send(U32, c, n, ms), r => IO.pure(U32, sent_poll(n, r)))
    case False{} True{}:
      IO.bind(Maybe<&1, U32>, U32, Chan.recv(U32, c), r => IO.pure(U32, received(r)))
    case False{} False{}:
      IO.bind(Poll<&1, &1, Maybe<&1, U32>, Unit>, U32, Chan.try_recv(U32, c, ms), r => IO.pure(U32, received_poll(r)))

def action(c: Chan(U32), senders: Bool, n: U32, +ms: U32) -> IO(U32):
  action_go(c, senders, n, ms, U32.is_eq(ms, 0))

def worker(c: Chan(U32), ready: Chan(Unit), senders: Bool, n: U32, ms: U32) -> IO(U32):
  do IO<U32>:
    Result<&1, &1, Unit, Unit> <- Chan.send(Unit, ready, Unit{})
    action(c, senders, n, ms)

def launch(c: Chan(U32), +ready: Chan(Unit), senders: Bool, n: U32, ms: U32) -> IO(Chan(U32)):
  do IO<Chan(U32)>:
    done : Chan(U32) <- IO.fork(U32, worker(c, ready, senders, n, ms))
    Maybe<&1, Unit> <- Chan.recv(Unit, ready)
    return done

def join(done: Chan(U32), want: U32) -> IO(Unit):
  do IO<Unit>:
    got : U32 <- IO.join(U32, done)
    check(got, want)

def wake(c: Chan(U32), senders: Bool, +n: U32) -> IO(Unit):
  do IO<Unit>:
    got : U32 <- action(c, Bool.not(senders), n, 0)
    check(got, n)

# Remove the head twice. The second timeout needs the new head's prev.
def head(+c: Chan(U32), +ready: Chan(Unit), +senders: Bool) -> IO(Unit):
  do IO<Unit>:
    a : Chan(U32) <- launch(c, ready, senders, 1, 20)
    b : Chan(U32) <- launch(c, ready, senders, 2, 20)
    d : Chan(U32) <- launch(c, ready, senders, 3, 0)
    Unit <- join(a, 200)
    Unit <- join(b, 200)
    Unit <- wake(c, senders, 3)
    Unit <- join(d, 3)
    Chan.close(U32, c)

# Remove a middle waiter and then the tail. Append and remove a new tail.
def middle(+c: Chan(U32), +ready: Chan(Unit), +senders: Bool) -> IO(Unit):
  do IO<Unit>:
    a : Chan(U32) <- launch(c, ready, senders, 1, 0)
    b : Chan(U32) <- launch(c, ready, senders, 2, 20)
    d : Chan(U32) <- launch(c, ready, senders, 3, 20)
    Unit <- join(b, 200)
    Unit <- join(d, 200)
    e : Chan(U32) <- launch(c, ready, senders, 4, 20)
    Unit <- join(e, 200)
    Unit <- wake(c, senders, 1)
    Unit <- join(a, 1)
    Chan.close(U32, c)

# Wake the head before the next timeout. Close blocking and timed waiters.
def after_wake(+c: Chan(U32), +ready: Chan(Unit), +senders: Bool) -> IO(Unit):
  do IO<Unit>:
    a : Chan(U32) <- launch(c, ready, senders, 1, 0)
    b : Chan(U32) <- launch(c, ready, senders, 2, 20)
    d : Chan(U32) <- launch(c, ready, senders, 3, 0)
    Unit <- wake(c, senders, 1)
    Unit <- join(a, 1)
    Unit <- join(b, 200)
    e : Chan(U32) <- launch(c, ready, senders, 4, 5000)
    Unit <- Chan.close(U32, c)
    Unit <- join(d, 100)
    join(e, 100)

# One activation parks again after a wake, then after its own timeout.
def again(+c: Chan(U32), +ready: Chan(Unit), +senders: Bool) -> IO(U32):
  do IO<U32>:
    got : U32 <- worker(c, ready, senders, 1, 0)
    Unit <- check(got, 1)
    late : U32 <- worker(c, ready, senders, 2, 20)
    Unit <- check(late, 200)
    worker(c, ready, senders, 3, 20)

def reuse(+c: Chan(U32), +ready: Chan(Unit), +senders: Bool) -> IO(Unit):
  do IO<Unit>:
    done : Chan(U32) <- IO.fork(U32, again(c, ready, senders))
    Maybe<&1, Unit> <- Chan.recv(Unit, ready)
    Unit <- wake(c, senders, 1)
    Maybe<&1, Unit> <- Chan.recv(Unit, ready)
    Maybe<&1, Unit> <- Chan.recv(Unit, ready)
    Unit <- join(done, 200)
    Chan.close(U32, c)

def test(body: Chan(U32) -> Chan(Unit) -> IO(Unit)) -> IO(Unit):
  do IO<Unit>:
    c : Chan(U32) <- Chan.new(U32, 0)
    ready : Chan(Unit) <- Chan.new(Unit, 1)
    body(c, ready)

def run(+senders: Bool, label: String) -> IO(Unit):
  do IO<Unit>:
    Unit <- test(c => r => head(c, r, senders))
    Unit <- test(c => r => middle(c, r, senders))
    Unit <- test(c => r => after_wake(c, r, senders))
    Unit <- test(c => r => reuse(c, r, senders))
    IO.print(label)

def main() -> IO(Unit):
  do IO<Unit>:
    Unit <- run(True{}, "send waiters")
    run(False{}, "receive waiters")

#|send waiters
#|receive waiters
