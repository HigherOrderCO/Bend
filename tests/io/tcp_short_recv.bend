# io (io_tcp_short_recv): the stream half of SPEC 1.8's read laws.
# A six-byte send read at max 3 answers "abc" and KEEPS the
# remainder in the stream, so the next recv answers "def" -- the
# datagram contrast of io_udp_truncate: a stream discards nothing.
# After the peer's orderly close a recv answers Done{None{}} ("ended"
# prints), the law's own words, never a Fail: a kit turning the
# zero-byte read into an error reds the last row.
# Handles thread back OUTSIDE the Result, tuples destructure in
# their own helpers, and the listener retries upward from a base
# port (+port licenses the port's reuse). The recv buffer allocates
# through the codec guard (the csrc pin holds this file's recv
# site).
import Base

law send_go:
  for m: Socket & Result<&1, &1, (U32 & String) & String, Unit>
  IO(Socket)

def send_go(m):
  (s, r) = m
  match r:
    case Done{u}:
      IO.pure(Socket, s)
    case Fail{((code, message), rest)}:
      do IO<Socket>:
        u : Unit <- IO.die(Unit, code, message)
        return s

law step_four:
  for l: Listener
  for m: Socket & Result<&1, &1, (U32 & String), Maybe<&1, String>>
  IO(U32)

def step_four(l, m):
  (a, r) = m
  do IO<U32>:
    d3 : Maybe<&1, String> <- IO.pass(Maybe<&1, String>, r)
    w  : Unit <- IO.print(Maybe.default(&1, String, d3, "ended"))
    y  : Unit <- Socket.close(a)
    z  : Unit <- Listener.close(l)
    return 0

law step_three:
  for l: Listener
  for c: Socket
  for m: Socket & Result<&1, &1, (U32 & String), Maybe<&1, String>>
  IO(U32)

def step_three(l, c, m):
  (a, r) = m
  do IO<U32>:
    d2 : Maybe<&1, String> <- IO.pass(Maybe<&1, String>, r)
    w  : Unit <- IO.print(Maybe.default(&1, String, d2, "ended"))
    x  : Unit <- Socket.close(c)
    rm : Socket & Result<&1, &1, (U32 & String), Maybe<&1, String>> <- TCP.recv(a, 64)
    step_four(l, rm)

law step_two:
  for l: Listener
  for c: Socket
  for m: Socket & Result<&1, &1, (U32 & String), Maybe<&1, String>>
  IO(U32)

def step_two(l, c, m):
  (a, r) = m
  do IO<U32>:
    d1 : Maybe<&1, String> <- IO.pass(Maybe<&1, String>, r)
    w  : Unit <- IO.print(Maybe.default(&1, String, d1, "ended"))
    rm : Socket & Result<&1, &1, (U32 & String), Maybe<&1, String>> <- TCP.recv(a, 64)
    step_three(l, c, rm)

law step_accept:
  for c: Socket
  for p: Listener & Result<&1, &1, (U32 & String), Socket>
  IO(U32)

def step_accept(c, p):
  (l, ar) = p
  do IO<U32>:
    a  : Socket <- IO.pass(Socket, ar)
    sm : Socket & Result<&1, &1, (U32 & String) & String, Unit> <- TCP.send(c, "abcdef")
    c2 : Socket <- send_go(sm)
    rm : Socket & Result<&1, &1, (U32 & String), Maybe<&1, String>> <- TCP.recv(a, 3)
    step_two(l, c2, rm)

law main_go:
  for b: Listener & U32
  IO(U32)

def main_go(b):
  (l, port) = b
  do IO<U32>:
    c : Socket <- IO.try(Socket, TCP.connect("127.0.0.1", port))
    p : Listener & Result<&1, &1, (U32 & String), Socket> <- TCP.accept(l)
    step_accept(c, p)

law tcp_retry:
  for port : U32
  for r    : Result<&1, &1, (U32 & String), Listener>
  for rest : IO(Listener & U32)
  IO(Listener & U32)

def tcp_retry(port, r, rest):
  match r:
    case Done{l}:
      IO.pure(Listener & U32, (l, port))
    case Fail{e}:
      rest

law tcp_listen_any:
  for tries : Nat
  for +port : U32
  IO(Listener & U32)

def tcp_listen_any(tries, port):
  match tries:
    case 0n:
      IO.die(Listener & U32, 1, "no free tcp port")
    case 1n+t:
      rest = tcp_listen_any(t, (port + 1 : U32))
      do IO<Listener & U32>:
        r : Result<&1, &1, (U32 & String), Listener> <- TCP.listen("127.0.0.1", port)
        tcp_retry(port, r, rest)

law main:
  IO(U32)

def main():
  do IO<U32>:
    b : Listener & U32 <- tcp_listen_any(32n, 19647)
    main_go(b)

#|abc
#|def
#|ended
