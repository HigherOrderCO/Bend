# io (io_nat_host_neg): a foreign effect answers a negative Nat. The C
# lane reads it as a word past the cap and fail-stops when Nat.show
# computes with it; the JS lane rejects it as the answer comes in. Both
# stop with the same message and exit code.
import Base

law get:
  IO(Nat)

def get():
  import "./nat_host_neg.c"
  import "./nat_host_neg.js"

def main() -> IO(Unit):
  IO.bind(Nat, Unit, get(), n => IO.print(Nat.show(n)))

#|bend: a Nat past the largest immediate 2^48-1
#|exit 1
