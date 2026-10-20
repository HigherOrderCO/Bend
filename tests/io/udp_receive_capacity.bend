# A large receive limit must preserve the datagram; smaller limits truncate.
import Base

def sent(m: Socket & Result<&1, &1, (U32 & String) & String, Unit>) -> IO(Socket):
  (s, r) = m
  match r:
    case Done{u}: IO.pure(Socket, s)
    case Fail{((code, message), rest)}: IO.die(Socket, code, message)

def received(m: Socket & Result<&1, &1, U32 & String, String & U32 & String>) -> IO(Socket):
  (s, r) = m
  match r:
    case Done{(host, port, data)}:
      do IO<Socket>:
        IO.print(data)
        return s
    case Fail{(code, message)}: IO.die(Socket, code, message)

def run(a: Socket & U32) -> IO(Unit):
  (receiver, +port) = a
  do IO<Unit>:
    sender : Socket <- IO.try(Socket, UDP.bind("127.0.0.1", 0))
    p : Socket & Result<&1, &1, (U32 & String) & String, Unit> <- UDP.send_to(sender, "127.0.0.1", port, "x")
    sender : Socket <- sent(p)
    p : Socket & Result<&1, &1, U32 & String, String & U32 & String> <- UDP.recv_from(receiver, 4294967295)
    receiver : Socket <- received(p)
    p : Socket & Result<&1, &1, (U32 & String) & String, Unit> <- UDP.send_to(sender, "127.0.0.1", port, "abcdef")
    sender : Socket <- sent(p)
    p : Socket & Result<&1, &1, U32 & String, String & U32 & String> <- UDP.recv_from(receiver, 3)
    receiver : Socket <- received(p)
    Socket.close(sender)
    Socket.close(receiver)

def retry(port: U32, r: Result<&1, &1, U32 & String, Socket>, rest: IO(Socket & U32)) -> IO(Socket & U32):
  match r:
    case Done{s}: IO.pure(Socket & U32, (s, port))
    case Fail{e}: rest

def bind(tries: Nat, +port: U32) -> IO(Socket & U32):
  match tries:
    case 0n: IO.die(Socket & U32, 1, "no free UDP port")
    case 1n+t:
      rest = bind(t, (port + 1 : U32))
      do IO<Socket & U32>:
        r : Result<&1, &1, U32 & String, Socket> <- UDP.bind("127.0.0.1", port)
        retry(port, r, rest)

def main() -> IO(Unit):
  do IO<Unit>:
    a : Socket & U32 <- bind(32n, 32400)
    run(a)

#|x
#|abc
