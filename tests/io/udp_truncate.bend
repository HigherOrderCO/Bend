# io (io_udp_truncate): SPEC 1.8's datagram boundary -- a long
# datagram truncates, never splits. Two datagrams queue on the bound
# socket; recv_from at max 3 answers the first one's head ("012")
# and its tail is discarded with it, so the next recv answers the
# second datagram whole ("tail-intact"), never the remainder: the
# pin holds both kits to the datagram boundary, and a buffered
# rewrite that carved streams out of datagrams reds the second row
# (a kit reading past max reds the first). Handles thread back
# OUTSIDE the Result, tuples destructure in their own helpers, and
# the binder retries upward from a base port (+port licenses the
# port's reuse). The recv buffers allocate through the codec guard
# (the csrc pin holds this file's recv site).
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

law data_end:
  for s: Socket
  for p: String & U32 & String
  IO(Socket & String)

def data_end(s, p):
  (host, from, data) = p
  IO.pure(Socket & String, (s, data))

law data_go:
  for m: Socket & Result<&1, &1, (U32 & String), String & U32 & String>
  IO(Socket & String)

def data_go(m):
  (s, r) = m
  do IO<Socket & String>:
    p : String & U32 & String <- IO.pass(String & U32 & String, r)
    data_end(s, p)

law recv_end:
  for s2 : Socket
  for q  : Socket & String
  IO(U32)

def recv_end(s2, q):
  (s1, d2) = q
  do IO<U32>:
    w : Unit <- IO.print(d2)
    x : Unit <- Socket.close(s1)
    y : Unit <- Socket.close(s2)
    return 0

law recv_two:
  for s2 : Socket
  for q  : Socket & String
  IO(U32)

def recv_two(s2, q):
  (s1, d1) = q
  do IO<U32>:
    w  : Unit <- IO.print(d1)
    m  : Socket & Result<&1, &1, (U32 & String), String & U32 & String> <- UDP.recv_from(s1, 64)
    q2 : Socket & String <- data_go(m)
    recv_end(s2, q2)

law recv_one:
  for s1: Socket
  for s2: Socket
  IO(U32)

def recv_one(s1, s2):
  do IO<U32>:
    m : Socket & Result<&1, &1, (U32 & String), String & U32 & String> <- UDP.recv_from(s1, 3)
    q : Socket & String <- data_go(m)
    recv_two(s2, q)

law sends_end:
  for p1: U32
  for s1: Socket
  for s2: Socket
  IO(U32)

def sends_end(p1, s1, s2):
  do IO<U32>:
    sm  : Socket & Result<&1, &1, (U32 & String) & String, Unit> <- UDP.send_to(s2, "127.0.0.1", p1, "tail-intact")
    s2b : Socket <- send_go(sm)
    recv_one(s1, s2b)

law main_go2:
  for +p1 : U32
  for s1  : Socket
  for b   : Socket & U32
  IO(U32)

def main_go2(p1, s1, b):
  (s2, p2) = b
  do IO<U32>:
    sm  : Socket & Result<&1, &1, (U32 & String) & String, Unit> <- UDP.send_to(s2, "127.0.0.1", p1, "0123456789")
    s2b : Socket <- send_go(sm)
    sends_end(p1, s1, s2b)

law main_go:
  for a: Socket & U32
  for b: Socket & U32
  IO(U32)

def main_go(a, b):
  (s1, p1) = a
  main_go2(p1, s1, b)

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
    a : Socket & U32 <- udp_bind_any(32n, 19213)
    b : Socket & U32 <- udp_bind_any(32n, 19253)
    main_go(a, b)

#|012
#|tail-intact
