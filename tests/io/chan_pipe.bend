# io (io_chan_pipe): a bounded channel between two computations. The
# producer sends 4..0 into a channel with room for two, so its fourth
# send parks until the consumer takes one; the consumer prints each
# value and, after the producer closes the channel, receives None and
# prints end. A Chan handle is Data: +ch copies it for both sides.
import Base

law produce:
  for +c: Chan(Nat)
  for n: Nat
  IO(Unit)

def produce(c, n):
  match n:
    case 0n:
      Chan.close(Nat, c)
    case 1n+p:
      +q  = p
      do IO<Unit>:
        Result<&1, &1, Nat, Unit> <- Chan.send(Nat, c, q)
        produce(c, q)

law consume.got:
  for rest: IO(Unit)
  for got: Maybe<&1, Nat>
  IO(Unit)

def consume.got(rest, got):
  match got:
    case None{}:
      IO.print("end")
    case Some{value}:
      IO.bind(Unit, Unit, IO.print(Nat.show(value)), u => rest)

law consume:
  for +c: Chan(Nat)
  for f: Nat
  IO(Unit)

def consume(c, f):
  match f:
    case 0n:
      IO.pure(Unit, Unit{})
    case 1n+g:
      rest = consume(c, g)
      IO.bind(Maybe<&1, Nat>, Unit, Chan.recv(Nat, c), consume.got(rest))

law main.go:
  for c: Chan(Nat)
  IO(U32)

def main.go(c):
  +ch = c
  do IO<U32>:
    Unit <- IO.spawn(Unit, produce(ch, 5n))
    Unit <- consume(ch, 10n)
    return 0

law main:
  IO(U32)

def main():
  IO.bind(Chan(Nat), U32, Chan.new(Nat, 2), main.go)

#|4
#|3
#|2
#|1
#|0
#|end
