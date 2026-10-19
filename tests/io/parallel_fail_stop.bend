# io (io_parallel_fail_stop): a fail-stop on a pool thread writes ONE
# clean line. The line is built in a stack buffer and sent with a single
# write, so a line is under PIPE_BUF and cannot interleave with another
# thread's line, and a short write is resumed, so the line is never cut.

import Base

law boom:
  IO(U32)

def boom():
  do IO<U32>:
    u : U32 <- IO.pure(U32, U32.from_nat(Nat.mul(U32.to_nat(16777216), U32.to_nat(16777216))))
    return u

law main:
  IO(U32)

def main():
  do IO<U32>:
    c : Chan(U32) <- IO.fork(U32, boom())
    x : U32 <- IO.join(U32, c)
    return 0

#|bend: a Nat past the largest immediate 2^48-1
#|exit 1
