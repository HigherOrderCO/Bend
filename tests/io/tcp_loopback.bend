# io (io_tcp_loopback): bend3 ran listen/connect/accept/send/recv over
# loopback end to end; bend's kit runs the same, sequentially -- the
# event loop blocks per op, and loopback completes each one at once.
# Handles are affine, so every op threads its handle back OUTSIDE the
# Result -- no path can lose it -- and each tuple destructures in its
# own helper (a destructure cannot ride a do line). The listener
# retries upward from a base port on Fail: the fallback action is
# built before the attempt, so the retry is a plain value and the
# recursion is a structural descent on tries (+port licenses the
# port's reuse). The recv buffer allocates through the codec
# guard (the csrc pin holds this file's only recv site).
import Base

law step_finish:
  for l: Listener
  for c: Socket
  for m: Socket & Result<&1, &1, (U32 & String), Maybe<&1, String>>
  IO(U32)

def step_finish(l, c, m):
  (a, r) = m
  do IO<U32>:
    data : Maybe<&1, String> <- IO.pass(Maybe<&1, String>, r)
    w    : Unit <- IO.print(Maybe.default(&1, String, data, "ended"))
    x    : Unit <- Socket.close(a)
    y    : Unit <- Socket.close(c)
    z    : Unit <- Listener.close(l)
    return 0

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

law step_accept:
  for c: Socket
  for p: Listener & Result<&1, &1, (U32 & String), Socket>
  IO(U32)

def step_accept(c, p):
  (l, ar) = p
  do IO<U32>:
    a  : Socket <- IO.pass(Socket, ar)
    sm : Socket & Result<&1, &1, (U32 & String) & String, Unit> <- TCP.send(c, "bend-tcp")
    c2 : Socket <- send_go(sm)
    rm : Socket & Result<&1, &1, (U32 & String), Maybe<&1, String>> <- TCP.recv(a, 64)
    step_finish(l, c2, rm)

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
    b : Listener & U32 <- tcp_listen_any(32n, 19587)
    main_go(b)

#|bend-tcp
