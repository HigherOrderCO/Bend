# io (io_tcp_backlog_timer): a backlog of queued connections does not keep
# the loop from its timers. An accept parks until its listener is
# readable, so each one passes through a poll, where a due timer fires:
# the 0 ms tick prints before the 20 queued connections are accepted.
# When an accept answered at once, the loop never polled until the
# backlog was gone, and the tick came last. udp_backlog_timer pins the
# same for UDP.recv_from.
import Base

law ping:
  for tag: String
  IO(Unit)

def ping(tag):
  do IO<Unit>:
    Unit <- IO.sleep(0)
    IO.print(tag)

law dials:
  for n : Nat
  for +p : U32
  IO(Unit)

def dials(n, p):
  match n:
    case 0n:
      IO.pure(Unit, Unit{})
    case 1n+k:
      do IO<Unit>:
        c : Socket <- IO.try(Socket, TCP.connect("127.0.0.1", p))
        u : Unit <- Socket.close(c)
        dials(k, p)

law accepts:
  for n : Nat
  for m : Listener & Result<&1, &1, (U32 & String), Socket>
  IO(Listener)

def accepts(n, m):
  match n:
    case 0n:
      (l, r) = m
      do IO<Listener>:
        a : Socket <- IO.pass(Socket, r)
        u : Unit <- Socket.close(a)
        return l
    case 1n+k:
      (l, r) = m
      do IO<Listener>:
        a  : Socket <- IO.pass(Socket, r)
        u  : Unit <- Socket.close(a)
        m2 : Listener & Result<&1, &1, (U32 & String), Socket> <- TCP.accept(l)
        accepts(k, m2)

law main_go:
  for b: Listener & U32
  IO(U32)

def main_go(b):
  (l, port) = b
  do IO<U32>:
    d  : Unit <- dials(20n, port)
    u  : Unit <- IO.spawn(Unit, ping("tick"))
    m  : Listener & Result<&1, &1, (U32 & String), Socket> <- TCP.accept(l)
    l2 : Listener <- accepts(19n, m)
    w  : Unit <- IO.print("drained")
    x  : Unit <- Listener.close(l2)
    return 0

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
    b : Listener & U32 <- tcp_listen_any(32n, 19687)
    main_go(b)

#|tick
#|drained
