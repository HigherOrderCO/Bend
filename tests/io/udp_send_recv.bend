# io (io_udp_send_recv): bend3's recv_from stamped each datagram with
# its sender, and send_to consumed exactly that stamp -- replying is
# echoing the address back. bend pins the same protocol whole: two
# sockets, a ping from the first, a reply from the second to the
# stamp it received, the pong printed off the first. Every op
# threads its handle back OUTSIDE the Result (no path can lose it),
# tuples destructure in helpers, binds retry upward on Fail: the
# fallback action is built before the attempt, so the retry is a
# plain value and the recursion is a structural descent on tries
# (+port licenses the port's reuse).
import Base

law send_go:
  for m: Socket & Result<&1, &1, (U32 & String) & String, Unit>
  IO(Socket)

def send_go(m):
  (s, r) = m
  match r:
    case Done{u}:
      IO.pure(Socket, s)
    case Fail{(error, data)}:
      (code, message) = error
      IO.die(Socket, code, message)

law finish_end:
  for s1 : Socket
  for s2 : Socket
  for p  : String & U32 & String
  IO(U32)

def finish_end(s1, s2, p):
  (host, from, data) = p
  do IO<U32>:
    w : Unit <- IO.print(data)
    x : Unit <- Socket.close(s1)
    y : Unit <- Socket.close(s2)
    return 0

law finish_go:
  for s2 : Socket
  for m  : Socket & Result<&1, &1, (U32 & String), String & U32 & String>
  IO(U32)

def finish_go(s2, m):
  (s1, r) = m
  do IO<U32>:
    p : String & U32 & String <- IO.pass(String & U32 & String, r)
    finish_end(s1, s2, p)

law reply_end:
  for s1 : Socket
  for s2 : Socket
  for p  : String & U32 & String
  IO(U32)

def reply_end(s1, s2, p):
  (host, from, data) = p
  do IO<U32>:
    sm  : Socket & Result<&1, &1, (U32 & String) & String, Unit> <- UDP.send_to(s2, host, from, String.append(data, "-pong"))
    s2b : Socket <- send_go(sm)
    rm  : Socket & Result<&1, &1, (U32 & String), String & U32 & String> <- UDP.recv_from(s1, 64)
    finish_go(s2b, rm)

law reply_go:
  for s1 : Socket
  for m  : Socket & Result<&1, &1, (U32 & String), String & U32 & String>
  IO(U32)

def reply_go(s1, m):
  (s2, r) = m
  do IO<U32>:
    p : String & U32 & String <- IO.pass(String & U32 & String, r)
    reply_end(s1, s2, p)

law main_go2:
  for s1 : Socket
  for b  : Socket & U32
  for data : String
  IO(U32)

def main_go2(s1, b, data):
  (s2, p2) = b
  do IO<U32>:
    sm  : Socket & Result<&1, &1, (U32 & String) & String, Unit> <- UDP.send_to(s1, "127.0.0.1", p2, data)
    s1b : Socket <- send_go(sm)
    m   : Socket & Result<&1, &1, (U32 & String), String & U32 & String> <- UDP.recv_from(s2, 64)
    reply_go(s1b, m)

law main_go:
  for a: Socket & U32
  for b: Socket & U32
  for data: String
  IO(U32)

def main_go(a, b, data):
  (s1, p1) = a
  main_go2(s1, b, data)

law udp_retry:
  for port : U32
  for r    : Result<&1, &1, (U32 & String), Socket>
  for rest : IO(Socket & U32)
  IO(Socket & U32)

def udp_retry(port, r, rest):
  match r:
    case Done{s}:
      IO.pure(Socket & U32, (s, port))
    case Fail{e}:
      rest

law udp_bind_any:
  for tries : Nat
  for +port : U32
  IO(Socket & U32)

def udp_bind_any(tries, port):
  match tries:
    case 0n:
      IO.die(Socket & U32, 1, "no free udp port")
    case 1n+t:
      rest = udp_bind_any(t, (port + 1 : U32))
      do IO<Socket & U32>:
        r : Result<&1, &1, (U32 & String), Socket> <- UDP.bind("127.0.0.1", port)
        udp_retry(port, r, rest)

law main:
  IO(U32)

def main():
  do IO<U32>:
    a : Socket & U32 <- udp_bind_any(32n, 19053)
    b : Socket & U32 <- udp_bind_any(32n, 19073)
    r : U32 <- main_go(a, b, "bend-udp")
    c : Socket & U32 <- udp_bind_any(32n, 19053)
    d : Socket & U32 <- udp_bind_any(32n, 19073)
    main_go(c, d, "")

#|bend-udp-pong
#|-pong
