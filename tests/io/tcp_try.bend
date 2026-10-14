# io (io_tcp_try): the try_ twins of TCP.accept, TCP.recv and TCP.send.
# try_accept with ms 0 and nobody dialing answers Wait{} at once; once a
# client dials, a 1 s try_accept answers Ready with the socket. A 5 ms
# try_recv with nothing sent answers Wait{} and leaves the stream as it
# was, so a recv after the peer sends "late" reads it whole; once the
# peer closes, a recv answers None{}. On a second connection whose peer
# never reads, try_send of 64 KiB chunks with a 20 ms limit goes until
# the kernel's buffers fill, then answers Wait{rest}: rest is the part of
# the chunk the kernel did not take, never empty. Handles thread back
# OUTSIDE the Poll, and the listener retries upward from a base port.
import Base

def chunk(n: Nat, acc: String) -> String:
  match n:
    case 0n:
      acc
    case 1n+p:
      chunk(p, String.append("0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef", acc))

def sent(m: Socket & Result<&1, &1, (U32 & String) & String, Unit>) -> IO(Socket):
  (s, r) = m
  match r:
    case Done{u}:
      IO.pure(Socket, s)
    case Fail{((code, message), rest)}:
      do IO<Socket>:
        u : Unit <- IO.die(Unit, code, message)
        return s

def got(m: Socket & Result<&1, &1, U32 & String, Maybe<&1, String>>) -> IO(Socket):
  (s, r) = m
  do IO<Socket>:
    d : Maybe<&1, String> <- IO.pass(Maybe<&1, String>, r)
    u : Unit <- IO.print("recv: " ++ Maybe.default(&1, String, d, "ended"))
    return s

# the first try_accept: nobody has dialed, so ms 0 answers Wait{}
def idle_accept(m: Listener & Poll<&1, &1, Result<&1, &1, U32 & String, Socket>, Unit>) -> IO(Listener):
  (l, r) = m
  match r:
    case Wait{u}:
      do IO<Listener>:
        v : Unit <- IO.print("accept: wait")
        return l
    case Ready{Done{s}}:
      do IO<Listener>:
        v : Unit <- Socket.close(s)
        w : Unit <- IO.print("accept: early")
        return l
    case Ready{Fail{(code, message)}}:
      do IO<Listener>:
        v : Unit <- IO.print(message)
        return l

# a 5 ms try_recv with nothing sent answers Wait{}
def idle_recv(m: Socket & Poll<&1, &1, Result<&1, &1, U32 & String, Maybe<&1, String>>, Unit>) -> IO(Socket):
  (s, r) = m
  match r:
    case Wait{u}:
      do IO<Socket>:
        v : Unit <- IO.print("try_recv: wait")
        return s
    case Ready{x}:
      do IO<Socket>:
        v : Unit <- IO.print("try_recv: early")
        return s

def fill.got(rest: Socket -> IO(Socket), m: Socket & Poll<&1, &1, Result<&1, &1, (U32 & String) & String, Unit>, String>) -> IO(Socket):
  (s, r) = m
  match r:
    case Ready{Done{u}}:
      rest(s)
    case Ready{Fail{((code, message), left)}}:
      do IO<Socket>:
        u : Unit <- IO.print("send: failed, " ++ message)
        return s
    case Wait{SCon{h, t}}:
      do IO<Socket>:
        u : Unit <- IO.print("send: wait, rest kept")
        return s
    case Wait{SNil{}}:
      do IO<Socket>:
        u : Unit <- IO.print("send: wait, rest empty")
        return s

# try_send 64 KiB chunks, at most k of them, until one would wait
def fill(k: Nat, s: Socket) -> IO(Socket):
  match k:
    case 0n:
      do IO<Socket>:
        u : Unit <- IO.print("send: never waited")
        return s
    case 1n+p:
      do IO<Socket>:
        m : Socket & Poll<&1, &1, Result<&1, &1, (U32 & String) & String, Unit>, String> <- TCP.try_send(s, chunk(1024n, ""), 20)
        fill.got(s2 => fill(p, s2), m)

def fill_go(c: Socket, m: Listener & Result<&1, &1, U32 & String, Socket>) -> IO(Unit):
  (l, r) = m
  do IO<Unit>:
    a  : Socket <- IO.pass(Socket, r)
    a2 : Socket <- fill(256n, a)
    x  : Unit <- Socket.close(a2)
    y  : Unit <- Socket.close(c)
    Listener.close(l)

# a second connection whose client c never reads
def fill_test(l: Listener, port: U32) -> IO(Unit):
  do IO<Unit>:
    c : Socket <- IO.try(Socket, TCP.connect("127.0.0.1", port))
    m : Listener & Result<&1, &1, U32 & String, Socket> <- TCP.accept(l)
    fill_go(c, m)

def talk(l: Listener, port: U32, c: Socket, a: Socket) -> IO(Unit):
  do IO<Unit>:
    m  : Socket & Poll<&1, &1, Result<&1, &1, U32 & String, Maybe<&1, String>>, Unit> <- TCP.try_recv(a, 64, 5)
    a1 : Socket <- idle_recv(m)
    sm : Socket & Result<&1, &1, (U32 & String) & String, Unit> <- TCP.send(c, "late")
    c1 : Socket <- sent(sm)
    rm : Socket & Result<&1, &1, U32 & String, Maybe<&1, String>> <- TCP.recv(a1, 64)
    a2 : Socket <- got(rm)
    x  : Unit <- Socket.close(c1)
    em : Socket & Result<&1, &1, U32 & String, Maybe<&1, String>> <- TCP.recv(a2, 64)
    a3 : Socket <- got(em)
    y  : Unit <- Socket.close(a3)
    fill_test(l, port)

# the second try_accept: c has dialed, so a 1 s try answers the socket
def took(port: U32, c: Socket, m: Listener & Poll<&1, &1, Result<&1, &1, U32 & String, Socket>, Unit>) -> IO(Unit):
  (l, r) = m
  match r:
    case Ready{Done{a}}:
      do IO<Unit>:
        u : Unit <- IO.print("accept: ready")
        talk(l, port, c, a)
    case Ready{Fail{(code, message)}}:
      IO.die(Unit, code, message)
    case Wait{u}:
      IO.die(Unit, 1, "accept: wait")

def main_go(b: Listener & U32) -> IO(Unit):
  (l, +port) = b
  do IO<Unit>:
    m0 : Listener & Poll<&1, &1, Result<&1, &1, U32 & String, Socket>, Unit> <- TCP.try_accept(l, 0)
    l1 : Listener <- idle_accept(m0)
    c  : Socket <- IO.try(Socket, TCP.connect("127.0.0.1", port))
    m1 : Listener & Poll<&1, &1, Result<&1, &1, U32 & String, Socket>, Unit> <- TCP.try_accept(l1, 1000)
    took(port, c, m1)

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
    b : Listener & U32 <- tcp_listen_any(32n, 20117)
    main_go(b)

#|accept: wait
#|accept: ready
#|try_recv: wait
#|recv: late
#|recv: ended
#|send: wait, rest kept
