# Echo server: binds port 7777 and sends every connection back what
# it says. Each accepted socket goes to a computation of its own
# (IO.spawn) that echoes each recv until the peer closes; the accept
# loop is @unsafe: it runs until killed. Try it:
#
#   bend demos/io_tcp_echos/main.bend -o echos && ./echos
#   nc 127.0.0.1 7777
import Base

# the move on a recv: the data sent back, or nothing: the connection
# closes at the peer's end (None{}) or on a failed recv
def echo_step(r: Result<&1, &1, U32 & String, Maybe<&1, String>>) -> Maybe<String>:
  match r:
    case Done{m}:
      m
    case Fail{e}:
      None{}

def echo_sent(m: Socket & Result<&1, &1, (U32 & String) & String, Unit>, next: Socket -> IO(Unit)) -> IO(Unit):
  (s, r) = m
  match r:
    case Done{u}:
      next(s)
    case Fail{e}:
      Socket.close(s)

# the move made: the data sent and the handler goes on, or the close
def echo_move(s: Socket, m: Maybe<String>, next: Socket -> IO(Unit)) -> IO(Unit):
  match m:
    case None{}:
      Socket.close(s)
    case Some{d}:
      do IO<Unit>:
        sm : Socket & Result<&1, &1, (U32 & String) & String, Unit> <- TCP.send(s, d)
        echo_sent(sm, next)

def echo_back(m: Socket & Result<&1, &1, U32 & String, Maybe<&1, String>>, next: Socket -> IO(Unit)) -> IO(Unit):
  (s, r) = m
  echo_move(s, echo_step(r), next)

@unsafe
def echo_conn(s: Socket) -> IO(Unit):
  do IO<Unit>:
    rm : Socket & Result<&1, &1, U32 & String, Maybe<&1, String>> <- TCP.recv(s, 4096)
    echo_back(rm, s2 => echo_conn(s2))

def serve_next(m: Listener & Result<&1, &1, U32 & String, Socket>) -> IO(Listener):
  (l, r) = m
  do IO<Listener>:
    s : Socket <- IO.pass(Socket, r)
    IO.spawn(Unit, echo_conn(s))
    return l

@unsafe
def serve(l: Listener) -> IO(Unit):
  do IO<Unit>:
    m  : Listener & Result<&1, &1, U32 & String, Socket> <- TCP.accept(l)
    l2 : Listener <- serve_next(m)
    serve(l2)

def main() -> IO(Unit):
  do IO<Unit>:
    l : Listener <- IO.try(Listener, TCP.listen("127.0.0.1", 7777))
    IO.print("echoing on 127.0.0.1:7777")
    serve(l)
