# A text send can stop inside UTF-8. Its byte residue resumes losslessly.
import Base
import ./tcp_try_bytes.bend as T

def Host.small_send(s: Socket) -> IO(Socket):
  import "./tcp_text_residue.c"
  import "./tcp_text_residue.js"

def euro(i: U32) -> U32:
  match i:
    case 0:
      226
    case 1:
      130
    case _:
      172

def want(i: U32) -> U32:
  match i:
    case 0:
      33
    case 1:
      33
    case _:
      euro(((i - 2) % 3 : U32))

def scan(xs: List<&2, U32>, +n: U32, good: Bool) -> U32 & Bool:
  match xs:
    case []:
      (n, good)
    case x <> rest:
      scan(rest, (n + 1 : U32), Bool.and(good, U32.is_eq(x, want(n))))

def verdict(good: Bool) -> String:
  match good:
    case True{}:
      "exact UTF-8"
    case False{}:
      "corrupt UTF-8"

def drain_got(rest: Socket -> (U32 & Bool) -> IO(Unit), s: Socket, +n: U32, good: Bool, data: Maybe<&1, List<&2, U32>>) -> IO(Unit):
  match data:
    case None{}:
      do IO<Unit>:
        Unit <- Socket.close(s)
        IO.print(verdict(Bool.and(good, U32.is_eq(n, 600002))))
    case Some{xs}:
      rest(s, scan(xs, n, good))

def drain_recv(rest: Socket -> (U32 & Bool) -> IO(Unit), state: U32 & Bool, m: Socket & Result<&1, &1, U32 & String, Maybe<&1, List<&2, U32>>>) -> IO(Unit):
  (n, good) = state
  (s, r) = m
  do IO<Unit>:
    data : Maybe<&1, List<&2, U32>> <- IO.pass(Maybe<&1, List<&2, U32>>, r)
    drain_got(rest, s, n, good, data)

def drain(fuel: Nat, s: Socket, state: U32 & Bool) -> IO(Unit):
  match fuel:
    case 0n:
      IO.die(Unit, 1, "read budget exhausted")
    case 1n+p:
      IO.bind(Socket & Result<&1, &1, U32 & String, Maybe<&1, List<&2, U32>>>, Unit,
        TCP.recv_bytes(s, 8192), m => drain_recv(s2 => st => drain(p, s2, st), state, m))

def sent(m: Socket & Result<&1, &1, (U32 & String) & List<&2, U32>, Unit>) -> IO(Unit):
  (s, r) = m
  match r:
    case Done{u}:
      Socket.close(s)
    case Fail{((code, message), rest)}:
      IO.die(Unit, code, message)

# The residue must start inside a character, or the test proves nothing.
def split(rest: List<&2, U32>) -> List<&2, U32> & Bool:
  match rest:
    case []:
      ([], False{})
    case +x <> xs:
      (x <> xs, Bool.and(U32.is_ge(x, 128), U32.is_lt(x, 192)))

def resume_split(peer: Socket, s: Socket, m: List<&2, U32> & Bool) -> IO(Unit):
  (rest, inside) = m
  match inside:
    case True{}:
      do IO<Unit>:
        Unit <- IO.spawn(Unit, drain(600003n, peer, (0, True{})))
        next : Socket & Result<&1, &1, (U32 & String) & List<&2, U32>, Unit> <- TCP.send_bytes(s, rest)
        sent(next)
    case False{}:
      IO.die(Unit, 1, "send stopped on a character boundary")

def resume(peer: Socket, m: Socket & Poll<&1, &1, Result<&1, &1, (U32 & String) & List<&2, U32>, Unit>, List<&2, U32>>) -> IO(Unit):
  (s, r) = m
  match r:
    case Wait{rest}:
      resume_split(peer, s, split(rest))
    case Ready{Done{u}}:
      do IO<Unit>:
        Unit <- IO.print("send did not wait")
        Unit <- Socket.close(s)
        drain(600003n, peer, (0, True{}))
    case Ready{Fail{((code, message), rest)}}:
      IO.die(Unit, code, message)

def accepted(c: Socket, m: Listener & Result<&1, &1, U32 & String, Socket>) -> IO(Unit):
  (l, r) = m
  do IO<Unit>:
    peer : Socket <- IO.pass(Socket, r)
    Unit <- Listener.close(l)
    c : Socket <- Host.small_send(c)
    result : Socket & Poll<&1, &1, Result<&1, &1, (U32 & String) & List<&2, U32>, Unit>, List<&2, U32>> <- TCP.try_send(c, "!!" ++ String.repeat("€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€", 6250n), 0)
    resume(peer, result)

def connected(bound: Listener & U32) -> IO(Unit):
  (l, port) = bound
  do IO<Unit>:
    c : Socket <- IO.try(Socket, TCP.connect("127.0.0.1", port))
    m : Listener & Result<&1, &1, U32 & String, Socket> <- TCP.accept(l)
    accepted(c, m)

def main() -> IO(Unit):
  IO.bind(Listener & U32, Unit, T.tcp_listen_any(32n, 20881), connected)

#|exact UTF-8
