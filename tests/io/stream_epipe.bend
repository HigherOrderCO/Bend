# io (io_stream_epipe): a write to stdout or stderr whose reader has
# left (EPIPE) ends the program quietly by SIGPIPE, status 141 in a
# shell, as yes | head does (#1467): it printed "bend: a short write
# on a standard stream" and exited 1. Epipe.write runs a child that
# writes a line to fd through the runtime's own writer, into a pipe
# whose read end is closed, and answers the child's status. A message
# from the child would land in this output. The first native child resumes
# here before stdout has a buffer, so IO.print buffers into its pipe
# and IO.exit must detect EPIPE while flushing it.
import Base

def Epipe.write(fd: U32, resume: U32) -> IO(U32):
  import "./stream_epipe.c"
  import "./stream_epipe.js"

def Epipe.done(code: U32) -> IO(Unit):
  match code:
    case 0:
      do IO<Unit>:
        IO.print("buffered")
        IO.exit(7)
    case _:
      IO.print("exit: " ++ U32.show(code))

def main() -> IO(Unit):
  do IO<Unit>:
    c : U32 <- Epipe.write(1, 1)
    Epipe.done(c)
    a : U32 <- Epipe.write(1, 0)
    u : Unit <- IO.print("stdout: " ++ U32.show(a))
    b : U32 <- Epipe.write(2, 0)
    IO.print("stderr: " ++ U32.show(b))

#|exit: 141
#|stdout: 141
#|stderr: 141
