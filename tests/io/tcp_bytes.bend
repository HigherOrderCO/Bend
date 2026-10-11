# io (io_tcp_bytes): TCP.send_bytes and TCP.recv_bytes carry bytes as they
# are, 0..255 one List cell each, where TCP.send and TCP.recv carry UTF-8.
# The client sends "O", the two bytes of "á" (C3 A1), 255 and 0; the
# server reads 2 bytes, which split the "á", then the rest, and every byte
# survives. A value past 255 fails with EINVAL (22) before any byte is sent,
# and the list comes back whole.
import Base

def bytes_line(xs: List<&2, U32>) -> String:
  match xs:
    case []:
      ""
    case [x]:
      U32.show(x)
    case x <> rest:
      U32.show(x) ++ " " ++ bytes_line(rest)

def got_line(m: Maybe<&1, List<&2, U32>>) -> String:
  match m:
    case Some{xs}:
      bytes_line(xs)
    case None{}:
      "ended"

def recv_go(m: Socket & Result<&1, &1, U32 & String, Maybe<&1, List<&2, U32>>>) -> IO(Socket):
  (s, r) = m
  do IO<Socket>:
    got : Maybe<&1, List<&2, U32>> <- IO.pass(Maybe<&1, List<&2, U32>>, r)
    u : Unit <- IO.print(got_line(got))
    return s

def send_go(m: Socket & Result<&1, &1, (U32 & String) & List<&2, U32>, Unit>) -> IO(Socket):
  (s, r) = m
  match r:
    case Done{u}:
      IO.pure(Socket, s)
    case Fail{((code, message), rest)}:
      do IO<Socket>:
        v : Unit <- IO.die(Unit, code, message)
        return s

def send_bad(m: Socket & Result<&1, &1, (U32 & String) & List<&2, U32>, Unit>) -> IO(Socket):
  (s, r) = m
  match r:
    case Done{u}:
      do IO<Socket>:
        v : Unit <- IO.print("unexpected send")
        return s
    case Fail{((code, message), rest)}:
      do IO<Socket>:
        v : Unit <- IO.print(U32.show(code) ++ ", kept " ++ bytes_line(rest))
        return s

def talk(c: Socket, p: Listener & Result<&1, &1, U32 & String, Socket>) -> IO(Unit):
  (l2, ar) = p
  do IO<Unit>:
    a  : Socket <- IO.pass(Socket, ar)
    s1 : Socket & Result<&1, &1, (U32 & String) & List<&2, U32>, Unit> <- TCP.send_bytes(c, [79, 195, 161, 255, 0])
    c1 : Socket <- send_go(s1)
    r1 : Socket & Result<&1, &1, U32 & String, Maybe<&1, List<&2, U32>>> <- TCP.recv_bytes(a, 2)
    a1 : Socket <- recv_go(r1)
    r2 : Socket & Result<&1, &1, U32 & String, Maybe<&1, List<&2, U32>>> <- TCP.recv_bytes(a1, 64)
    a2 : Socket <- recv_go(r2)
    s2 : Socket & Result<&1, &1, (U32 & String) & List<&2, U32>, Unit> <- TCP.send_bytes(c1, [1, 256])
    c2 : Socket <- send_bad(s2)
    x  : Unit <- Socket.close(a2)
    y  : Unit <- Socket.close(c2)
    Listener.close(l2)

def main_go(b: Listener & U32) -> IO(Unit):
  (l, port) = b
  do IO<Unit>:
    c : Socket <- IO.try(Socket, TCP.connect("127.0.0.1", port))
    p : Listener & Result<&1, &1, U32 & String, Socket> <- TCP.accept(l)
    talk(c, p)

def tcp_retry(port: U32, r: Result<&1, &1, U32 & String, Listener>, rest: IO(Listener & U32)) ->
  IO(Listener & U32):
  match r:
    case Done{l}:
      IO.pure(Listener & U32, (l, port))
    case Fail{e}:
      rest

def tcp_listen_any(tries: Nat, +port: U32) -> IO(Listener & U32):
  match tries:
    case 0n:
      IO.die(Listener & U32, 1, "no free tcp port")
    case 1n+t:
      rest = tcp_listen_any(t, (port + 1 : U32))
      do IO<Listener & U32>:
        r : Result<&1, &1, U32 & String, Listener> <- TCP.listen("127.0.0.1", port)
        tcp_retry(port, r, rest)

def main() -> IO(Unit):
  do IO<Unit>:
    b : Listener & U32 <- tcp_listen_any(32n, 19917)
    main_go(b)

#|79 195
#|161 255 0
#|22, kept 1 256
