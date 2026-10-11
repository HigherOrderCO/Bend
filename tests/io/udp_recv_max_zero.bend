# io (io_udp_recv_max_zero): a UDP receive of at most 0 bytes fails with
# EINVAL at once, as every receive does. recvfrom(2) of 0 bytes would
# take a waiting datagram and drop it, and on a quiet socket macOS answers
# it with no datagram at all. try_recv_from and recv_from with max 0 fail
# on a quiet socket without waiting; then a datagram is sent, and a
# recv_from with max 0 fails again and leaves it, so a recv_from of 64
# still reads "hello". The sockets retry upward from a base port on Fail,
# as udp_try_recv_from does.
import Base

def show_fail(e: U32 & String) -> String:
  (code, message) = e
  U32.show(code) ++ (" " ++ message)

def show_got(r: Result<&1, &1, U32 & String, String & U32 & String>) -> String:
  match r:
    case Done{(host, port, data)}:
      "got " ++ data
    case Fail{e}:
      show_fail(e)

def show_poll(r: Poll<&1, &1, Result<&1, &1, U32 & String, String & U32 & String>, Unit>) -> String:
  match r:
    case Ready{g}:
      show_got(g)
    case Wait{u}:
      "wait"

def told(tag: String, m: Socket & String) -> IO(Socket):
  (s, line) = m
  do IO<Socket>:
    Unit <- IO.print(tag ++ line)
    return s

def got(m: Socket & Result<&1, &1, U32 & String, String & U32 & String>) -> Socket & String:
  (s, r) = m
  (s, show_got(r))

def poll(m: Socket & Poll<&1, &1, Result<&1, &1, U32 & String, String & U32 & String>, Unit>) -> Socket & String:
  (s, r) = m
  (s, show_poll(r))

def sent(m: Socket & Result<&1, &1, (U32 & String) & String, Unit>) -> IO(Socket):
  (s, r) = m
  match r:
    case Done{u}:
      IO.pure(Socket, s)
    case Fail{((code, message), rest)}:
      IO.die(Socket, code, message)

def run(a: Socket & U32, b: Socket & U32) -> IO(U32):
  (s1, p1) = a
  (s2, p2) = b
  do IO<U32>:
    m0 : Socket & Poll<&1, &1, Result<&1, &1, U32 & String, String & U32 & String>, Unit> <- UDP.try_recv_from(s2, 0, 50)
    t1 : Socket <- told("try_recv_from: ", poll(m0))
    m1 : Socket & Result<&1, &1, U32 & String, String & U32 & String> <- UDP.recv_from(t1, 0)
    t2 : Socket <- told("recv_from: ", got(m1))
    m2 : Socket & Result<&1, &1, (U32 & String) & String, Unit> <- UDP.send_to(s1, "127.0.0.1", p2, "hello")
    u1 : Socket <- sent(m2)
    Unit <- IO.sleep(20)
    m3 : Socket & Result<&1, &1, U32 & String, String & U32 & String> <- UDP.recv_from(t2, 0)
    t3 : Socket <- told("queued: ", got(m3))
    m4 : Socket & Result<&1, &1, U32 & String, String & U32 & String> <- UDP.recv_from(t3, 64)
    t4 : Socket <- told("then recv_from 64: ", got(m4))
    Unit <- Socket.close(u1)
    Unit <- Socket.close(t4)
    return 0

def udp_retry(port: U32, r: Result<&1, &1, U32 & String, Socket>, rest: IO(Socket & U32)) -> IO(Socket & U32):
  match r:
    case Done{s}:
      IO.pure(Socket & U32, (s, port))
    case Fail{e}:
      rest

def udp_bind_any(tries: Nat, +port: U32) -> IO(Socket & U32):
  match tries:
    case 0n:
      IO.die(Socket & U32, 1, "no free udp port")
    case 1n+t:
      rest = udp_bind_any(t, (port + 1 : U32))
      do IO<Socket & U32>:
        r : Result<&1, &1, U32 & String, Socket> <- UDP.bind("127.0.0.1", port)
        udp_retry(port, r, rest)

def main() -> IO(U32):
  do IO<U32>:
    a : Socket & U32 <- udp_bind_any(32n, 19193)
    b : Socket & U32 <- udp_bind_any(32n, 19233)
    run(a, b)

#|try_recv_from: 22 Invalid argument
#|recv_from: 22 Invalid argument
#|queued: 22 Invalid argument
#|then recv_from 64: got hello
