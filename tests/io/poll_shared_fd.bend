# io (io_poll_shared_fd): two waiters share a readable socket. The first
# consumes a, reparks, and must not displace the second waiter for b. Its
# next wake consumes c: registration order and re-arming both survive.
import Base

def Shared.read() -> IO(U32):
  import "./poll_shared_fd.c"
  import "./poll_shared_fd.js"

def main() -> IO(Unit):
  do IO<Unit>:
    n : U32 <- Shared.read()
    IO.print(U32.show(n))

#|3
