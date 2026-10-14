# io (io_udp_recv_park): a recv_from on a quiet socket parks its
# computation and nothing else waits: main's three 1 ms ticks land
# while the receiver sits parked, then main's datagram wakes it and
# it prints. The socket is non-blocking and the receive runs on the
# loop's thread after the park, re-parking on a datagram that vanished
# after poll reported it, instead of holding a helper or the loop.
import Base

law got_go:
  for s2 : Socket
  for p  : String & U32 & String
  IO(Unit)

def got_go(s2, p):
  (host, from, data) = p
  do IO<Unit>:
    w : Unit <- IO.print(data)
    Socket.close(s2)

law got:
  for m : Socket & Result<&1, &1, (U32 & String), String & U32 & String>
  IO(Unit)

def got(m):
  (s2, r) = m
  do IO<Unit>:
    p : String & U32 & String <- IO.pass(String & U32 & String, r)
    got_go(s2, p)

law receive:
  for s2 : Socket
  IO(Unit)

def receive(s2):
  do IO<Unit>:
    m : Socket & Result<&1, &1, (U32 & String), String & U32 & String> <-
      UDP.recv_from(s2, 64)
    got(m)

law ticks:
  for n : Nat
  IO(Unit)

def ticks(n):
  match n:
    case 0n:
      IO.pure(Unit, Unit{})
    case 1n+p:
      do IO<Unit>:
        u : Unit <- IO.sleep(1)
        w : Unit <- IO.print("tick")
        ticks(p)

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

law main_go:
  for a: Socket & U32
  for b: Socket & U32
  IO(U32)

def main_go(a, b):
  (s1, p1) = a
  (s2, p2) = b
  do IO<U32>:
    u   : Unit <- IO.spawn(Unit, receive(s2))
    t   : Unit <- ticks(3n)
    sm  : Socket & Result<&1, &1, (U32 & String) & String, Unit> <-
      UDP.send_to(s1, "127.0.0.1", p2, "bend-park")
    s1b : Socket <- send_go(sm)
    x   : Unit <- Socket.close(s1b)
    return 0

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
    a : Socket & U32 <- udp_bind_any(32n, 19193)
    b : Socket & U32 <- udp_bind_any(32n, 19293)
    main_go(a, b)

#|tick
#|tick
#|tick
#|bend-park
