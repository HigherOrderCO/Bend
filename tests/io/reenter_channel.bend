# Nested foreign IO wakes a parked outer receiver, sender or timed waiter.
# Their continuations run only after the inner scheduler has returned.
import Base

def Host.channel(c: Chan(U32), mode: U32) -> IO(Unit):
  import "./reenter_channel.js"

def show(x: Maybe<&1, U32>) -> String:
  match x:
    case Some{value}:
      U32.show(value)
    case None{}:
      "closed"

def poll(x: Poll<&1, &1, Maybe<&1, U32>, Unit>) -> String:
  match x:
    case Ready{value}:
      show(value)
    case Wait{u}:
      "timeout"

def child(c: Chan(U32), mode: U32) -> IO(Unit):
  match mode:
    case 0:
      do IO<Unit>:
        x : Maybe<&1, U32> <- Chan.recv(U32, c)
        IO.print(show(x))
    case 2:
      do IO<Unit>:
        x : Result<&1, &1, U32, Unit> <- Chan.send(U32, c, 11)
        IO.print("sent")
    case _:
      do IO<Unit>:
        x : Poll<&1, &1, Maybe<&1, U32>, Unit> <- Chan.try_recv(U32, c, 5000)
        IO.print(poll(x))

def go(c: Chan(U32), +mode: U32) -> IO(Unit):
  +ch = c
  do IO<Unit>:
    Unit <- IO.spawn(Unit, child(ch, mode))
    Unit <- IO.sleep(1)
    Unit <- Host.channel(ch, mode)
    IO.print("outer " ++ U32.show(mode))

def run(+mode: U32) -> IO(Unit):
  IO.bind(Chan(U32), Unit, Chan.new(U32, 0), c => go(c, mode))

def main() -> IO(Unit):
  do IO<Unit>:
    Unit <- run(0)
    Unit <- IO.sleep(1)
    Unit <- run(1)
    Unit <- IO.sleep(1)
    Unit <- run(2)
    Unit <- IO.sleep(1)
    run(3)

#|outer 0
#|7
#|outer 1
#|7
#|outer 2
#|sent
#|outer 3
#|closed
