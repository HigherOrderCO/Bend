# io (io_tcp_spawn): a server that serves connections concurrently.
# main listens, spawns an accept loop that spawns one echo handler
# per connection (recv, send back with an echo: prefix, close), forks
# three clients that connect, send their tag and read the reply, and
# joins them in order: every reply lands whatever order the OS
# delivers the connections, since each handler is its own
# computation and an accept or recv that would block parks only its
# own computation. The listener retries upward from a base port.
import Base

law handle.sent:
  for m: Socket & Result<&1, &1, (U32 & String) & String, Unit>
  IO(Unit)

def handle.sent(m):
  (s, r) = m
  Socket.close(s)

law handle.read:
  for m: Socket & Result<&1, &1, (U32 & String), Maybe<&1, String>>
  IO(Unit)

def handle.read(m):
  (s, r) = m
  do IO<Unit>:
    data : Maybe<&1, String> <- IO.pass(Maybe<&1, String>, r)
    sent : Socket & Result<&1, &1, (U32 & String) & String, Unit> <-
      TCP.send(s, String.append("echo:", Maybe.default(&1, String, data, "")))
    handle.sent(sent)

law handle:
  for s: Socket
  IO(Unit)

def handle(s):
  do IO<Unit>:
    m : Socket & Result<&1, &1, (U32 & String), Maybe<&1, String>> <- TCP.recv(s, 8)
    handle.read(m)

law serve.got:
  for rest: Listener -> IO(Unit)
  for m: Listener & Result<&1, &1, (U32 & String), Socket>
  IO(Unit)

def serve.got(rest, m):
  (l, r) = m
  do IO<Unit>:
    s : Socket <- IO.pass(Socket, r)
    Unit <- IO.spawn(Unit, handle(s))
    rest(l)

law serve:
  for n: Nat
  for l: Listener
  IO(Unit)

def serve(n, l):
  match n:
    case 0n:
      Listener.close(l)
    case 1n+p:
      IO.bind(Listener & Result<&1, &1, (U32 & String), Socket>, Unit,
        TCP.accept(l), serve.got(l2 => serve(p, l2)))

law client.read:
  for m: Socket & Result<&1, &1, (U32 & String), Maybe<&1, String>>
  IO(String)

def client.read(m):
  (s, r) = m
  do IO<String>:
    reply : Maybe<&1, String> <- IO.pass(Maybe<&1, String>, r)
    Unit <- Socket.close(s)
    return Maybe.default(&1, String, reply, "ended")

law client.sent:
  for m: Socket & Result<&1, &1, (U32 & String) & String, Unit>
  IO(String)

def client.sent(m):
  (s, r) = m
  match r:
    case Done{u}:
      IO.bind(Socket & Result<&1, &1, (U32 & String), Maybe<&1, String>>, String,
        TCP.recv(s, 64), client.read)
    case Fail{((code, message), rest)}:
      do IO<String>:
        Unit <- Socket.close(s)
        IO.die(String, code, message)

law client:
  for port: U32
  for tag: String
  IO(String)

def client(port, tag):
  do IO<String>:
    s : Socket <- IO.try(Socket, TCP.connect("127.0.0.1", port))
    m : Socket & Result<&1, &1, (U32 & String) & String, Unit> <- TCP.send(s, tag)
    client.sent(m)

law main.go:
  for b: Listener & U32
  IO(U32)

def main.go(b):
  (l, +port) = b
  do IO<U32>:
    Unit <- IO.spawn(Unit, serve(3n, l))
    a : Chan(String) <- IO.fork(String, client(port, "one"))
    b : Chan(String) <- IO.fork(String, client(port, "two"))
    c : Chan(String) <- IO.fork(String, client(port, "three"))
    x : String <- IO.join(String, a)
    y : String <- IO.join(String, b)
    z : String <- IO.join(String, c)
    Unit <- IO.print(x)
    Unit <- IO.print(y)
    Unit <- IO.print(z)
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
    b : Listener & U32 <- tcp_listen_any(32n, 19747)
    main.go(b)

#|echo:one
#|echo:two
#|echo:three
