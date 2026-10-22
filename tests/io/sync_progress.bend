# A runnable child must progress while its parent keeps making ready effects.
# No timer, host scheduling, or elapsed-time threshold decides the result.
import Base

def busy(n: Nat) -> IO(Unit):
  match n:
    case 0n:
      IO.pure(Unit, Unit{})
    case 1n+p:
      do IO<Unit>:
        t : Nat <- IO.now()
        busy(p)

def report(r: Poll<&1, &1, Maybe<&1, U32>, Unit>) -> IO(Unit):
  match r:
    case Ready{Some{v}}:
      IO.print(U32.show(v))
    case Ready{None{}}:
      IO.die(Unit, 1, "closed")
    case Wait{u}:
      IO.die(Unit, 1, "spawned computation was starved")

def main() -> IO(Unit):
  do IO<Unit>:
    +flag : Chan(U32) <- Chan.new(U32, 1)
    u : Unit <- IO.spawn(Result<&1, &1, U32, Unit>, Chan.send(U32, flag, 73))
    v : Unit <- busy(4096n)
    r : Poll<&1, &1, Maybe<&1, U32>, Unit> <- Chan.try_recv(U32, flag, 0)
    report(r)

#|73
