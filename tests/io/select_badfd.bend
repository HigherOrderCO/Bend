# io (io_select_badfd): a foreign fill hands back real closed fds. A
# receive or accept parked on one answers Fail(EBADF), a try_ one
# Ready(Fail(EBADF)) well before its deadline; none aborts the loop or
# wakes the valid unwritten pipe parked by another computation. IO.die
# closes that idle pipe at process exit, as in select_eintr.bend. A
# connect parks on an fd of its own, which no fill can close, so it is
# not covered here.
import Base

def Readiness.closed() -> IO(Result<&1, &1, U32 & String, Socket>):
  import "./select_badfd.c"
  import "./select_badfd.js"

def Readiness.closed_listener() -> IO(Result<&1, &1, U32 & String, Listener>):
  import "./select_badfd.c"
  import "./select_badfd.js"

def Readiness.idle() -> IO(Unit):
  import "./select_badfd.c"
  import "./select_badfd.js"

def idle() -> IO(Unit):
  do IO<Unit>:
    Unit <- Readiness.idle()
    IO.die(Unit, 1, "valid idle descriptor woke")

def errno(e: U32 & String) -> String:
  (code, message) = e
  match code:
    case 9:
      "EBADF"
    case other:
      "wrong errno"

def got(r: Result<&1, &1, U32 & String, String & U32 & String>) -> String:
  match r:
    case Done{value}:
      "unexpected datagram"
    case Fail{e}:
      errno(e)

def recv(m: Socket & Result<&1, &1, U32 & String, String & U32 & String>) -> String:
  (kept, r) = m
  "recv_from: " ++ got(r)

def try_recv(m: Socket & Poll<&1, &1, Result<&1, &1, U32 & String, String & U32 & String>, Unit>) -> String:
  (kept, p) = m
  match p:
    case Ready{r}:
      "try_recv_from: Ready " ++ got(r)
    case Wait{u}:
      "try_recv_from: Wait"

def accept(m: Listener & Result<&1, &1, U32 & String, Socket>) -> String:
  (kept, r) = m
  match r:
    case Done{s}:
      "accept: unexpected connection"
    case Fail{e}:
      "accept: " ++ errno(e)

def main() -> IO(U32):
  do IO<U32>:
    Unit <- IO.spawn(Unit, idle())
    Unit <- IO.sleep(1)
    s1 : Socket <- IO.try(Socket, Readiness.closed())
    m1 : Socket & Result<&1, &1, U32 & String, String & U32 & String> <- UDP.recv_from(s1, 64)
    Unit <- IO.print(recv(m1))
    s2 : Socket <- IO.try(Socket, Readiness.closed())
    m2 : Socket & Poll<&1, &1, Result<&1, &1, U32 & String, String & U32 & String>, Unit> <- UDP.try_recv_from(s2, 64, 2000)
    Unit <- IO.print(try_recv(m2))
    l : Listener <- IO.try(Listener, Readiness.closed_listener())
    m3 : Listener & Result<&1, &1, U32 & String, Socket> <- TCP.accept(l)
    Unit <- IO.print(accept(m3))
    Unit <- IO.sleep(5)
    Unit <- IO.print("continued")
    IO.die(U32, 0, "")

#|recv_from: EBADF
#|try_recv_from: Ready EBADF
#|accept: EBADF
#|continued
