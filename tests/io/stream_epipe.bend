# io (io_stream_epipe): a write to stdout or stderr whose reader has
# left (EPIPE) ends the program quietly by SIGPIPE, status 141 in a
# shell, as yes | head does (#1467): it printed "bend: a short write
# on a standard stream" and exited 1. Epipe.write runs a child that
# writes a line to fd through the runtime's own writer, into a pipe
# whose read end is closed, and answers the child's status. A message
# from the child would land in this output.
import Base

def Epipe.write(fd: U32) -> IO(U32):
  import "./stream_epipe.c"
  import "./stream_epipe.js"

def main() -> IO(Unit):
  do IO<Unit>:
    a : U32 <- Epipe.write(1)
    u : Unit <- IO.print("stdout: " ++ U32.show(a))
    b : U32 <- Epipe.write(2)
    IO.print("stderr: " ++ U32.show(b))

#|stdout: 141
#|stderr: 141
