# io (io_nat_host_bounds): a foreign effect answers Nats at and past the
# immediate cap: 2^48-1, 2^48 and 2^53. The C lane takes each word as it
# is; the JS lane takes each number exactly, so the unchecked operations (a
# word, a quotient, a remainder) print the same on both lanes.
import Base

law get:
  for k: U32
  IO(Nat)

def get(k):
  import "./nat_host_bounds.c"
  import "./nat_host_bounds.js"

def show3(+n: Nat) -> IO(Unit):
  do IO<Unit>:
    a : Unit <- IO.print(U32.show(U32.from_nat(n)))
    b : Unit <- IO.print(Nat.show(Nat.div(n, U32.to_nat(1048576))))
    IO.print(Nat.show(Nat.mod(n, U32.to_nat(1000))))

def main() -> IO(Unit):
  do IO<Unit>:
    x : Nat <- get(0)
    u : Unit <- show3(x)
    y : Nat <- get(1)
    v : Unit <- show3(y)
    z : Nat <- get(2)
    show3(z)

#|4294967295
#|268435455
#|655
#|0
#|268435456
#|656
#|0
#|8589934592
#|992
