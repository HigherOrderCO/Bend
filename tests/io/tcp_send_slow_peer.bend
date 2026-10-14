# io (io_tcp_send_slow_peer): a peer that never reads must not stop the
# loop or hold a helper. 70 clients connect one after another and sit;
# the server sends 64 KiB chunks to each until the send fails, so 70
# senders sit on full sockets at once, more than the 64 helper threads.
# A send that would block parks its computation until the socket is
# writable, so main's print lands once every client is accepted and a
# File.open (a helper job) answers at once. Before this, the JS lane's
# send blocked the loop thread outright and the C lane's send held a
# helper, and the canary waited until the holders let go 1.5 s later.
# The holders still sit, so main ends the process with IO.die code 0.
import Base

def chunk(n: Nat, acc: String) -> String:
  match n:
    case 0n:
      acc
    case 1n+p:
      chunk(p, String.append("0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef", acc))

def blast.sent(rest: Socket -> IO(Unit), m: Socket & Result<&1, &1, (U32 & String) & String, Unit>) -> IO(Unit):
  (s, r) = m
  match r:
    case Done{u}:
      rest(s)
    case Fail{e}:
      Socket.close(s)

def blast(k: Nat, s: Socket) -> IO(Unit):
  match k:
    case 0n:
      Socket.close(s)
    case 1n+p:
      do IO<Unit>:
        m : Socket & Result<&1, &1, (U32 & String) & String, Unit> <- TCP.send(s, chunk(1024n, ""))
        blast.sent(s2 => blast(p, s2), m)

def serve.got(rest: Listener -> IO(Unit), m: Listener & Result<&1, &1, (U32 & String), Socket>) -> IO(Unit):
  (l, r) = m
  do IO<Unit>:
    s : Socket <- IO.pass(Socket, r)
    Unit <- IO.spawn(Unit, blast(256n, s))
    rest(l)

def serve(n: Nat, l: Listener) -> IO(Unit):
  match n:
    case 0n:
      Listener.close(l)
    case 1n+p:
      IO.bind(Listener & Result<&1, &1, (U32 & String), Socket>, Unit,
        TCP.accept(l), serve.got(l2 => serve(p, l2)))

def hold.got(r: Result<&1, &1, (U32 & String), Socket>) -> IO(Unit):
  match r:
    case Done{s}:
      do IO<Unit>:
        Unit <- IO.sleep(1500)
        Socket.close(s)
    case Fail{e}:
      IO.pure(Unit, Unit{})

def holders(port: U32, n: Nat) -> IO(Unit):
  match n:
    case 0n:
      IO.pure(Unit, Unit{})
    case 1n+p:
      +pt = port
      do IO<Unit>:
        r : Result<&1, &1, (U32 & String), Socket> <- TCP.connect("127.0.0.1", pt)
        Unit <- IO.spawn(Unit, hold.got(r))
        holders(pt, p)

def canary.read(m: File & Result<&1, &1, (U32 & String), String>) -> IO(Unit):
  (f, r) = m
  File.close(f)

def canary.judge(left: Nat) -> IO(Unit):
  match left:
    case 0n:
      IO.print("canary slow")
    case 1n+p:
      IO.print("canary fast")

def canary() -> IO(Unit):
  do IO<Unit>:
    t0 : Nat <- IO.now()
    f : File <- IO.try(File, File.open("/etc/hosts", "r"))
    m : File & Result<&1, &1, (U32 & String), String> <- File.read(f, 16)
    Unit <- canary.read(m)
    t1 : Nat <- IO.now()
    canary.judge(Nat.sub(500n, Nat.sub(t1, t0)))

def main.go(b: Listener & U32) -> IO(U32):
  (l, port) = b
  +p = port
  do IO<U32>:
    a : Chan(Unit) <- IO.fork(Unit, serve(70n, l))
    Unit <- holders(p, 70n)
    Unit <- IO.join(Unit, a)
    Unit <- IO.sleep(10)
    Unit <- IO.print("loop alive")
    Unit <- canary()
    IO.die(U32, 0, "")

def tcp_retry(port: U32, r: Result<&1, &1, (U32 & String), Listener>, rest: IO(Listener & U32)) -> IO(Listener & U32):
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
        r : Result<&1, &1, (U32 & String), Listener> <- TCP.listen("127.0.0.1", port)
        tcp_retry(port, r, rest)

def main() -> IO(U32):
  do IO<U32>:
    b : Listener & U32 <- tcp_listen_any(32n, 19847)
    main.go(b)

#|loop alive
#|canary fast
