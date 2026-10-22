# A ready callback can wake a channel whose timer is already in the ready batch.
import Base

def Host.receive(c: Chan(U32)) -> IO(Poll<&1, &1, Maybe<&1, U32>, Unit>):
  import "./channel_due.js"

def show(p: Poll<&1, &1, Maybe<&1, U32>, Unit>) -> String:
  match p:
    case Ready{Some{n}}:
      "got " ++ U32.show(n)
    case Ready{None{}}:
      "closed"
    case Wait{u}:
      "timeout"

def sleepy() -> IO(Unit):
  do IO<Unit>:
    Unit <- IO.sleep(50)
    IO.print("sleep-survived")

def main() -> IO(Unit):
  do IO<Unit>:
    c : Chan(U32) <- Chan.new(U32, 0)
    Unit <- IO.spawn(Unit, sleepy())
    p : Poll<&1, &1, Maybe<&1, U32>, Unit> <- Host.receive(c)
    Unit <- IO.print(show(p))
    IO.print("after")

#|got 7
#|after
#|sleep-survived
