# io (io_nat_host_neg): a foreign effect answers a negative Nat. The JS
# lane rejects it as the answer comes in, with the host range, not the
# 2^48-1 arithmetic limit. The C lane reads it as a word past the cap and
# fail-stops later, in Nat.show, with that limit, so it has no twin here.
import Base

law get:
  IO(Nat)

def get():
  import "./nat_host_neg.js"

def main() -> IO(Unit):
  IO.bind(Nat, Unit, get(), n => IO.print(Nat.show(n)))

#|bend: expected a Nat host number: an integer from 0 to 2^53
#|exit 1
