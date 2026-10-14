# io (io_tcp_try_recv): TCP.try_recv is recv with a deadline. It answers
# Wait{} at once when ms is 0 and nothing waits, Wait{} after ms when
# nothing arrives (the socket parks on the poller AND on the loop's clock,
# and the first to fire wakes it), and Ready{Done{Some{data}}} when data
# does. An accepted socket is tried empty at once, then tried empty for
# 3 ms with IO.now on both sides (a 2 ms floor shows the loop really
# waited, with room for a coarse clock), then the peer sends and a 1 s
# try answers the data. Every op threads its handle back OUTSIDE the
# Poll.
import Base

def waited(ok: Bool) -> String:
  match ok:
    case True{}:
      "waited"
    case False{}:
      "early"

# what a try answers: the data, or idle when it would have waited
law shown:
  for idle: String
  for r: Poll<&1, &1, Result<&1, &1, (U32 & String), Maybe<&1, String>>, Unit>
  String

def shown(idle, r):
  match r:
    case Ready{Done{Some{data}}}:
      data
    case Ready{Done{None{}}}:
      "ended"
    case Ready{Fail{(code, message)}}:
      message
    case Wait{u}:
      idle

law finish:
  for l: Listener
  for c: Socket
  for m: Socket & Poll<&1, &1, Result<&1, &1, (U32 & String), Maybe<&1, String>>, Unit>
  IO(U32)

def finish(l, c, m):
  (a, r) = m
  do IO<U32>:
    w   : Unit <- IO.print(shown("none", r))
    x   : Unit <- Socket.close(a)
    y   : Unit <- Socket.close(c)
    z   : Unit <- Listener.close(l)
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

# the third try: the peer has sent, so a 1 s try answers the data
law step_data:
  for l: Listener
  for c: Socket
  for a: Socket
  IO(U32)

def step_data(l, c, a):
  do IO<U32>:
    sm : Socket & Result<&1, &1, (U32 & String) & String, Unit> <- TCP.send(c, "bend-poll")
    c2 : Socket <- send_go(sm)
    m  : Socket & Poll<&1, &1, Result<&1, &1, (U32 & String), Maybe<&1, String>>, Unit> <- TCP.try_recv(a, 64, 1000)
    finish(l, c2, m)

# the second try: 3 ms with nothing sent answers Wait{}, and IO.now
# shows the loop waited for it
law step_timed_go:
  for l: Listener
  for c: Socket
  for t0: Nat
  for m: Socket & Poll<&1, &1, Result<&1, &1, (U32 & String), Maybe<&1, String>>, Unit>
  IO(U32)

def step_timed_go(l, c, t0, m):
  (a, r) = m
  do IO<U32>:
    t1  : Nat <- IO.now()
    w   : Unit <- IO.print(shown("timeout", r))
    v   : Unit <- IO.print(waited(Nat.is_ge(Nat.sub(t1, t0), 2n)))
    step_data(l, c, a)

law step_timed:
  for l: Listener
  for c: Socket
  for a: Socket
  IO(U32)

def step_timed(l, c, a):
  do IO<U32>:
    t0 : Nat <- IO.now()
    m  : Socket & Poll<&1, &1, Result<&1, &1, (U32 & String), Maybe<&1, String>>, Unit> <- TCP.try_recv(a, 64, 3)
    step_timed_go(l, c, t0, m)

# the first try: ms = 0 and nothing sent answers Wait{} at once
law step_empty:
  for l: Listener
  for c: Socket
  for m: Socket & Poll<&1, &1, Result<&1, &1, (U32 & String), Maybe<&1, String>>, Unit>
  IO(U32)

def step_empty(l, c, m):
  (a, r) = m
  do IO<U32>:
    w   : Unit <- IO.print(shown("none", r))
    step_timed(l, c, a)

law step_accept:
  for c: Socket
  for p: Listener & Result<&1, &1, (U32 & String), Socket>
  IO(U32)

def step_accept(c, p):
  (l, ar) = p
  do IO<U32>:
    a : Socket <- IO.pass(Socket, ar)
    m : Socket & Poll<&1, &1, Result<&1, &1, (U32 & String), Maybe<&1, String>>, Unit> <- TCP.try_recv(a, 64, 0)
    step_empty(l, c, m)

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
    b : Listener & U32 <- tcp_listen_any(32n, 19617)
    main_go(b)

#|none
#|timeout
#|waited
#|bend-poll
