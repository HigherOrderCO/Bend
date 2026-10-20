# Grow after a partially drained buffer wraps, fill to a non-power-of-two
# room, then close and drain. Every value must retain its FIFO position.
import Base

def check(ok: Bool) -> IO(Unit):
  match ok:
    case True{}:
      IO.pure(Unit, Unit{})
    case False{}:
      IO.die(Unit, 1, "wrong buffered value")

def sent(r: Result<&1, &1, U32, Unit>) -> IO(Unit):
  match r:
    case Done{u}:
      IO.pure(Unit, Unit{})
    case Fail{v}:
      IO.die(Unit, 1, "buffered send failed")

def got(want: U32, r: Maybe<&1, U32>) -> IO(Unit):
  match r:
    case Some{v}:
      check(U32.is_eq(v, want))
    case None{}:
      IO.die(Unit, 1, "buffer closed early")

def put(n: Nat, +c: Chan(U32), +next: U32) -> IO(Unit):
  match n:
    case 0n:
      IO.pure(Unit, Unit{})
    case 1n+p:
      do IO<Unit>:
        r : Result<&1, &1, U32, Unit> <- Chan.send(U32, c, next)
        u : Unit <- sent(r)
        put(p, c, U32.inc(next))

def take(n: Nat, +c: Chan(U32), +next: U32) -> IO(Unit):
  match n:
    case 0n:
      IO.pure(Unit, Unit{})
    case 1n+p:
      do IO<Unit>:
        r : Maybe<&1, U32> <- Chan.recv(U32, c)
        u : Unit <- got(next, r)
        take(p, c, U32.inc(next))

def full(r: Poll<&1, &1, Result<&1, &1, U32, Unit>, U32>) -> IO(Unit):
  match r:
    case Wait{v}:
      check(U32.is_eq(v, 99))
    case Ready{r}:
      IO.die(Unit, 1, "room was exceeded")

def ended(r: Maybe<&1, U32>) -> IO(Unit):
  match r:
    case None{}:
      IO.print("wrapped FIFO drained")
    case Some{v}:
      IO.die(Unit, 1, "closed buffer retained data")

def main() -> IO(Unit):
  do IO<Unit>:
    +c : Chan(U32) <- Chan.new(U32, 19)
    a : Unit <- put(12n, c, 0)
    b : Unit <- take(5n, c, 0)
    d : Unit <- put(12n, c, 12)
    r : Poll<&1, &1, Result<&1, &1, U32, Unit>, U32> <- Chan.try_send(U32, c, 99, 0)
    e : Unit <- full(r)
    f : Unit <- Chan.close(U32, c)
    g : Unit <- take(19n, c, 5)
    last : Maybe<&1, U32> <- Chan.recv(U32, c)
    ended(last)

#|wrapped FIFO drained
