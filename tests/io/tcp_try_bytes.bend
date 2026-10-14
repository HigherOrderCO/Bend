# io (io_tcp_try_bytes): the try_ twins of TCP.recv_bytes and
# TCP.send_bytes. try_recv_bytes with ms 0 and nothing sent answers Wait{};
# once the client sends 1 2 3, one bounded by 1 s answers them. A list with
# a value past 255 fails with EINVAL (22) through Ready, before any byte is
# sent, and comes back whole. Toward a client that never reads,
# try_send_bytes of 64 Ki bytes with a 20 ms limit goes until the kernel's
# buffers fill, then answers Wait{rest}: rest, the bytes the kernel did not
# take, is never empty.
import Base

def bytes_line(xs: List<&2, U32>) -> String:
  match xs:
    case []:
      ""
    case [x]:
      U32.show(x)
    case x <> rest:
      U32.show(x) ++ " " ++ bytes_line(rest)

def zeros(n: Nat, acc: List<&2, U32>) -> List<&2, U32>:
  match n:
    case 0n:
      acc
    case 1n+p:
      zeros(p, 0 <> acc)

def say(s: Socket, line: String) -> IO(Socket):
  do IO<Socket>:
    u : Unit <- IO.print(line)
    return s

def recv_line(r: Poll<&1, &1, Result<&1, &1, U32 & String, Maybe<&1, List<&2, U32>>>, Unit>) -> String:
  match r:
    case Ready{Done{Some{xs}}}:
      "try_recv_bytes: " ++ bytes_line(xs)
    case Ready{Done{None{}}}:
      "try_recv_bytes: ended"
    case Ready{Fail{(code, message)}}:
      "try_recv_bytes: " ++ message
    case Wait{u}:
      "try_recv_bytes: wait"

def recv_go(m: Socket & Poll<&1, &1, Result<&1, &1, U32 & String, Maybe<&1, List<&2, U32>>>, Unit>) -> IO(Socket):
  (s, r) = m
  say(s, recv_line(r))

def bad_line(r: Poll<&1, &1, Result<&1, &1, (U32 & String) & List<&2, U32>, Unit>, List<&2, U32>>) -> String:
  match r:
    case Ready{Fail{((code, message), kept)}}:
      "try_send_bytes: " ++ U32.show(code) ++ ", kept " ++ bytes_line(kept)
    case Ready{Done{u}}:
      "try_send_bytes: sent"
    case Wait{kept}:
      "try_send_bytes: wait"

def bad_go(m: Socket & Poll<&1, &1, Result<&1, &1, (U32 & String) & List<&2, U32>, Unit>, List<&2, U32>>) -> IO(Socket):
  (s, r) = m
  say(s, bad_line(r))

def sent(m: Socket & Result<&1, &1, (U32 & String) & List<&2, U32>, Unit>) -> IO(Socket):
  (s, r) = m
  match r:
    case Done{u}:
      IO.pure(Socket, s)
    case Fail{((code, message), rest)}:
      IO.die(Socket, code, message)

def fill.got(more: Socket -> IO(Socket), m: Socket & Poll<&1, &1, Result<&1, &1, (U32 & String) & List<&2, U32>, Unit>, List<&2, U32>>) -> IO(Socket):
  (s, r) = m
  match r:
    case Ready{Done{u}}:
      more(s)
    case Ready{Fail{((code, message), left)}}:
      say(s, "try_send_bytes: failed, " ++ message)
    case Wait{x <> rest}:
      say(s, "try_send_bytes: wait, rest kept")
    case Wait{[]}:
      say(s, "try_send_bytes: wait, rest empty")

# try_send_bytes the 64 Ki bytes in big, at most k times, until one waits
def fill(k: Nat, +big: List<&2, U32>, s: Socket) -> IO(Socket):
  match k:
    case 0n:
      say(s, "try_send_bytes: never waited")
    case 1n+p:
      do IO<Socket>:
        m : Socket & Poll<&1, &1, Result<&1, &1, (U32 & String) & List<&2, U32>, Unit>, List<&2, U32>> <- TCP.try_send_bytes(s, big, 20)
        fill.got(s2 => fill(p, big, s2), m)

def talk(l: Listener, c: Socket, a: Socket) -> IO(Unit):
  do IO<Unit>:
    m0 : Socket & Poll<&1, &1, Result<&1, &1, U32 & String, Maybe<&1, List<&2, U32>>>, Unit> <- TCP.try_recv_bytes(a, 16, 0)
    a1 : Socket <- recv_go(m0)
    s1 : Socket & Result<&1, &1, (U32 & String) & List<&2, U32>, Unit> <- TCP.send_bytes(c, [1, 2, 3])
    c1 : Socket <- sent(s1)
    m1 : Socket & Poll<&1, &1, Result<&1, &1, U32 & String, Maybe<&1, List<&2, U32>>>, Unit> <- TCP.try_recv_bytes(a1, 16, 1000)
    a2 : Socket <- recv_go(m1)
    b1 : Socket & Poll<&1, &1, Result<&1, &1, (U32 & String) & List<&2, U32>, Unit>, List<&2, U32>> <- TCP.try_send_bytes(a2, [1, 256], 0)
    a3 : Socket <- bad_go(b1)
    a4 : Socket <- fill(256n, zeros(65536n, []), a3)
    x  : Unit <- Socket.close(a4)
    y  : Unit <- Socket.close(c1)
    Listener.close(l)

def accepted(c: Socket, m: Listener & Result<&1, &1, U32 & String, Socket>) -> IO(Unit):
  (l, r) = m
  do IO<Unit>:
    a : Socket <- IO.pass(Socket, r)
    talk(l, c, a)

def main_go(b: Listener & U32) -> IO(Unit):
  (l, +port) = b
  do IO<Unit>:
    c : Socket <- IO.try(Socket, TCP.connect("127.0.0.1", port))
    m : Listener & Result<&1, &1, U32 & String, Socket> <- TCP.accept(l)
    accepted(c, m)

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
    b : Listener & U32 <- tcp_listen_any(32n, 20181)
    main_go(b)

#|try_recv_bytes: wait
#|try_recv_bytes: 1 2 3
#|try_send_bytes: 22, kept 1 256
#|try_send_bytes: wait, rest kept
