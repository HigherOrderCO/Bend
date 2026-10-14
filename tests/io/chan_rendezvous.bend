# io (io_chan_rendezvous): a channel with no room hands each value
# straight from a sender to a receiver. main parks on the first recv,
# the sender wakes it with 1 and parks on 2 (no receiver waits), main
# takes 2 from the parked sender and wakes it. The prints interleave
# as m0 s0 s1 m1 m2 s2; the sender's last print lands after main has
# answered, since the process runs until every computation ends.
import Base

law sender:
  for +c: Chan(U32)
  IO(Unit)

def sender(c):
  do IO<Unit>:
    Unit <- IO.print("s0")
    Result<&1, &1, U32, Unit> <- Chan.send(U32, c, 1)
    Unit <- IO.print("s1")
    Result<&1, &1, U32, Unit> <- Chan.send(U32, c, 2)
    IO.print("s2")

law main.go:
  for c: Chan(U32)
  IO(U32)

def main.go(c):
  +ch = c
  do IO<U32>:
    Unit <- IO.spawn(Unit, sender(ch))
    Unit <- IO.print("m0")
    a : Maybe<&1, U32> <- Chan.recv(U32, ch)
    Unit <- IO.print("m1")
    b : Maybe<&1, U32> <- Chan.recv(U32, ch)
    Unit <- IO.print("m2")
    return 0

law main:
  IO(U32)

def main():
  IO.bind(Chan(U32), U32, Chan.new(U32, 0), main.go)

#|m0
#|s0
#|s1
#|m1
#|m2
#|s2
