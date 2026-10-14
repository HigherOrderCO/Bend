# io (io_within_busy): a deadline wins while computations are busy. Two
# computations pass 500k values over a channel, so a step is always
# runnable; IO.within's 1 ms deadline still answers None{} before the
# pipeline ends, as the loop also looks at its waits while busy (#1122).
# The loser, a 50 ms sleep, runs to its end before the program exits.
import Base

def produce(+c: Chan(Nat), n: Nat) -> IO(Unit):
  match n:
    case 0n:
      Chan.close(Nat, c)
    case 1n+p:
      +q = p
      do IO<Unit>:
        Result<&1, &1, Nat, Unit> <- Chan.send(Nat, c, q)
        produce(c, q)

def alive(m: Maybe<&1, Nat>) -> Bool:
  match m:
    case None{}:
      False{}
    case Some{value}:
      True{}

def consume(f: Nat, +c: Chan(Nat), live: Bool) -> IO(Unit):
  match f live:
    case 0n _:
      IO.pure(Unit, Unit{})
    case 1n+g False{}:
      IO.print("pipeline done")
    case 1n+g True{}:
      IO.bind(Maybe<&1, Nat>, Unit, Chan.recv(Nat, c), got => consume(g, c, alive(got)))

def show(r: Maybe<&1, Unit>) -> IO(Unit):
  match r:
    case None{}:
      IO.print("deadline")
    case Some{u}:
      IO.print("slept")

def race() -> IO(Unit):
  IO.bind(Maybe<&1, Unit>, Unit, IO.within(Unit, 1, IO.sleep(50)), show)

def run(+ch: Chan(Nat)) -> IO(Unit):
  do IO<Unit>:
    Unit <- IO.spawn(Unit, race())
    Unit <- IO.spawn(Unit, produce(ch, 500000n))
    consume(500002n, ch, True{})

def go(ch: Chan(Nat)) -> IO(Unit):
  run(ch)

def main() -> IO(Unit):
  IO.bind(Chan(Nat), Unit, Chan.new(Nat, 0), go)

#|deadline
#|pipeline done
