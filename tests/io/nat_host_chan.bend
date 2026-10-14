# io (io_nat_host_chan): a Nat crosses a generic effect, a channel, and
# comes back unchanged on both lanes: the C lane passes the word as it
# is, the JS lane the number.
import Base

def got(m: Maybe<&1, Nat>) -> Nat:
  match m:
    case None{}:
      0n
    case Some{n}:
      n

def go(+c: Chan(Nat)) -> IO(Unit):
  do IO<Unit>:
    s : Result<&1, &1, Nat, Unit> <- Chan.send(Nat, c, Nat.add(Nat.pow(2n, 47n), Nat.sub(Nat.pow(2n, 47n), 1n)))
    m : Maybe<&1, Nat> <- Chan.recv(Nat, c)
    IO.print(Nat.show(got(m)))

def main() -> IO(Unit):
  IO.bind(Chan(Nat), Unit, Chan.new(Nat, 2), c => go(c))

#|281474976710655
