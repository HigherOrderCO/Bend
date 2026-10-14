# io (io_udp_backlog_timer): a backlog of queued datagrams does not keep
# the loop from its timers. A recv_from parks until its socket is
# readable, so each one passes through a poll, where a due timer fires:
# the 0 ms tick prints before the receiver drains its 40 datagrams. When
# a recv answered at once, the loop never polled until the backlog was
# gone, and the tick came last.
import Base

law ping:
  for tag: String
  IO(Unit)

def ping(tag):
  do IO<Unit>:
    Unit <- IO.sleep(0)
    IO.print(tag)

law sent:
  for r : Result<&1, &1, (U32 & String) & String, Unit>
  IO(Unit)

def sent(r):
  match r:
    case Done{u}:
      IO.pure(Unit, u)
    case Fail{(error, data)}:
      (code, message) = error
      IO.die(Unit, code, message)

law sends:
  for n : Nat
  for m : Socket & Result<&1, &1, (U32 & String) & String, Unit>
  for +p : U32
  IO(Socket)

def sends(n, m, p):
  match n:
    case 0n:
      (s, r) = m
      IO.pure(Socket, s)
    case 1n+k:
      (s, r) = m
      do IO<Socket>:
        u : Unit <- sent(r)
        m2 : Socket & Result<&1, &1, (U32 & String) & String, Unit> <-
          UDP.send_to(s, "127.0.0.1", p, "x")
        sends(k, m2, p)

law recvs:
  for n : Nat
  for m : Socket & Result<&1, &1, (U32 & String), String & U32 & String>
  IO(Socket)

def recvs(n, m):
  match n:
    case 0n:
      (s, r) = m
      IO.pure(Socket, s)
    case 1n+k:
      (s, r) = m
      do IO<Socket>:
        d : String & U32 & String <- IO.pass(String & U32 & String, r)
        m2 : Socket & Result<&1, &1, (U32 & String), String & U32 & String> <-
          UDP.recv_from(s, 8)
        recvs(k, m2)

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

law main_go:
  for a: Socket & U32
  for b: Socket & U32
  IO(U32)

def main_go(a, b):
  (s1, p1) = a
  (s2, p2) = b
  do IO<U32>:
    s1b : Socket <- sends(41n, (s1, Done{Unit{}}), p2)
    u   : Unit <- IO.spawn(Unit, ping("tick"))
    s2b : Socket <- recvs(41n, (s2, Done{("", 0, "")}))
    w   : Unit <- IO.print("drained")
    x   : Unit <- Socket.close(s1b)
    y   : Unit <- Socket.close(s2b)
    return 0

law main:
  IO(U32)

def main():
  do IO<U32>:
    a : Socket & U32 <- udp_bind_any(32n, 19393)
    b : Socket & U32 <- udp_bind_any(32n, 19493)
    main_go(a, b)

#|tick
#|drained
