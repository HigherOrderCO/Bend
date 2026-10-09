# io (io_select_badfd): a foreign fill hands back a real closed UDP fd.
# Its recv must answer Fail(EBADF), not abort the loop or wake the valid
# unwritten pipe parked by another computation. IO.die closes that idle
# pipe at process exit, as in select_eintr.bend.
import Base

def Readiness.closed() -> IO(Result<&1, &1, U32 & String, Socket>):
  import "./select_badfd.c"
  import "./select_badfd.js"

def Readiness.idle() -> IO(Unit):
  import "./select_badfd.c"
  import "./select_badfd.js"

def idle() -> IO(Unit):
  do IO<Unit>:
    Unit <- Readiness.idle()
    IO.die(Unit, 1, "valid idle descriptor woke")

def report_code(code: U32) -> IO(Unit):
  match code:
    case 9:
      IO.print("EBADF")
    case other:
      IO.die(Unit, 1, "wrong errno")

def report(r: Result<&1, &1, U32 & String, String & U32 & String>) -> IO(Unit):
  match r:
    case Done{value}:
      IO.die(Unit, 1, "unexpected datagram")
    case Fail{error}:
      (code, message) = error
      report_code(code)

def finish(m: Socket & Result<&1, &1, U32 & String, String & U32 & String>) -> IO(U32):
  (kept, r) = m
  do IO<U32>:
    Unit <- report(r)
    Unit <- IO.sleep(5)
    Unit <- IO.print("continued")
    IO.die(U32, 0, "")

def main() -> IO(U32):
  do IO<U32>:
    Unit <- IO.spawn(Unit, idle())
    Unit <- IO.sleep(1)
    s : Socket <- IO.try(Socket, Readiness.closed())
    m : Socket & Result<&1, &1, U32 & String, String & U32 & String> <- UDP.recv_from(s, 64)
    finish(m)

#|EBADF
#|continued
