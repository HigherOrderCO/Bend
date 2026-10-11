# io (io_udp_bytes): UDP.send_bytes_to and UDP.recv_bytes_from carry a
# datagram's bytes as they are, 0..255 one List cell each, where UDP.send_to
# and UDP.recv_from carry UTF-8. A datagram of 0, 255, the two bytes of "á"
# (C3 A1) and a lone 128 arrives byte for byte; the text "á" sent by
# UDP.send_to arrives as its two bytes; an empty datagram is an empty List; a
# max of 2 keeps the first 2 bytes of 5 and drops the rest, as UDP does. A
# value past 255 fails with EINVAL (22) before anything is sent, the list
# handed back whole, and so does a malformed address. The try_ twins answer
# through Poll: Wait{} on a quiet socket, Ready{Done{}} for a send and for
# the datagram it sent, Ready{Fail{}} with the list for a value past 255. A
# max of 0 fails with EINVAL, as in every receive.
import Base

def bytes_line(xs: List<&2, U32>) -> String:
  match xs:
    case []:
      "empty"
    case [x]:
      U32.show(x)
    case x <> rest:
      U32.show(x) ++ " " ++ bytes_line(rest)

def show_recv(m: Socket & Result<&1, &1, U32 & String, String & U32 & List<&2, U32>>) -> IO(Socket):
  (s, r) = m
  match r:
    case Done{(host, from, data)}:
      do IO<Socket>:
        p : Unit <- IO.print(bytes_line(data))
        return s
    case Fail{(code, message)}:
      do IO<Socket>:
        p : Unit <- IO.print("recv failed " ++ U32.show(code))
        return s

def show_try_recv(m: Socket & Poll<&1, &1, Result<&1, &1, U32 & String, String & U32 & List<&2, U32>>, Unit>) -> IO(Socket):
  (s, r) = m
  match r:
    case Ready{Done{(host, from, data)}}:
      do IO<Socket>:
        p : Unit <- IO.print("ready " ++ bytes_line(data))
        return s
    case Ready{Fail{(code, message)}}:
      do IO<Socket>:
        p : Unit <- IO.print("ready failed " ++ U32.show(code))
        return s
    case Wait{u}:
      do IO<Socket>:
        p : Unit <- IO.print("wait")
        return s

def send_go(m: Socket & Result<&1, &1, (U32 & String) & List<&2, U32>, Unit>) -> IO(Socket):
  (s, r) = m
  match r:
    case Done{u}:
      IO.pure(Socket, s)
    case Fail{((code, message), rest)}:
      do IO<Socket>:
        p : Unit <- IO.print(U32.show(code) ++ ", kept " ++ bytes_line(rest))
        return s

def text_go(m: Socket & Result<&1, &1, (U32 & String) & String, Unit>) -> IO(Socket):
  (s, r) = m
  match r:
    case Done{u}:
      IO.pure(Socket, s)
    case Fail{((code, message), rest)}:
      do IO<Socket>:
        v : Unit <- IO.die(Unit, code, message)
        return s

def show_try_send(m: Socket & Poll<&1, &1, Result<&1, &1, (U32 & String) & List<&2, U32>, Unit>, List<&2, U32>>) -> IO(Socket):
  (s, r) = m
  match r:
    case Ready{Done{u}}:
      do IO<Socket>:
        p : Unit <- IO.print("ready sent")
        return s
    case Ready{Fail{((code, message), rest)}}:
      do IO<Socket>:
        p : Unit <- IO.print("ready " ++ U32.show(code) ++ ", kept " ++ bytes_line(rest))
        return s
    case Wait{rest}:
      do IO<Socket>:
        p : Unit <- IO.print("wait, kept " ++ bytes_line(rest))
        return s

def run(a: Socket, b: Socket, +port: U32) -> IO(U32):
  do IO<U32>:
    m1  : Socket & Result<&1, &1, (U32 & String) & List<&2, U32>, Unit> <-
      UDP.send_bytes_to(a, "127.0.0.1", port, [0, 255, 195, 161, 128])
    a1  : Socket <- send_go(m1)
    r1  : Socket & Result<&1, &1, U32 & String, String & U32 & List<&2, U32>> <-
      UDP.recv_bytes_from(b, 64)
    b1  : Socket <- show_recv(r1)
    m2  : Socket & Result<&1, &1, (U32 & String) & String, Unit> <-
      UDP.send_to(a1, "127.0.0.1", port, "á")
    a2  : Socket <- text_go(m2)
    r2  : Socket & Result<&1, &1, U32 & String, String & U32 & List<&2, U32>> <-
      UDP.recv_bytes_from(b1, 64)
    b2  : Socket <- show_recv(r2)
    m3  : Socket & Result<&1, &1, (U32 & String) & List<&2, U32>, Unit> <-
      UDP.send_bytes_to(a2, "127.0.0.1", port, [])
    a3  : Socket <- send_go(m3)
    r3  : Socket & Result<&1, &1, U32 & String, String & U32 & List<&2, U32>> <-
      UDP.recv_bytes_from(b2, 64)
    b3  : Socket <- show_recv(r3)
    m4  : Socket & Result<&1, &1, (U32 & String) & List<&2, U32>, Unit> <-
      UDP.send_bytes_to(a3, "127.0.0.1", port, [1, 2, 3, 4, 5])
    a4  : Socket <- send_go(m4)
    r4  : Socket & Result<&1, &1, U32 & String, String & U32 & List<&2, U32>> <-
      UDP.recv_bytes_from(b3, 2)
    b4  : Socket <- show_recv(r4)
    m5  : Socket & Result<&1, &1, (U32 & String) & List<&2, U32>, Unit> <-
      UDP.send_bytes_to(a4, "127.0.0.1", port, [1, 256])
    a5  : Socket <- send_go(m5)
    m6  : Socket & Result<&1, &1, (U32 & String) & List<&2, U32>, Unit> <-
      UDP.send_bytes_to(a5, "1.2.3.", 9, [7, 8])
    a6  : Socket <- send_go(m6)
    r5  : Socket & Poll<&1, &1, Result<&1, &1, U32 & String, String & U32 & List<&2, U32>>, Unit> <-
      UDP.try_recv_bytes_from(b4, 64, 0)
    b5  : Socket <- show_try_recv(r5)
    m7  : Socket & Poll<&1, &1, Result<&1, &1, (U32 & String) & List<&2, U32>, Unit>, List<&2, U32>> <-
      UDP.try_send_bytes_to(a6, "127.0.0.1", port, [9, 200, 0], 50)
    a7  : Socket <- show_try_send(m7)
    r6  : Socket & Poll<&1, &1, Result<&1, &1, U32 & String, String & U32 & List<&2, U32>>, Unit> <-
      UDP.try_recv_bytes_from(b5, 64, 1000)
    b6  : Socket <- show_try_recv(r6)
    m8  : Socket & Poll<&1, &1, Result<&1, &1, (U32 & String) & List<&2, U32>, Unit>, List<&2, U32>> <-
      UDP.try_send_bytes_to(a7, "127.0.0.1", port, [300], 0)
    a8  : Socket <- show_try_send(m8)
    r7  : Socket & Poll<&1, &1, Result<&1, &1, U32 & String, String & U32 & List<&2, U32>>, Unit> <-
      UDP.try_recv_bytes_from(b6, 64, 20)
    b7  : Socket <- show_try_recv(r7)
    r8  : Socket & Result<&1, &1, U32 & String, String & U32 & List<&2, U32>> <-
      UDP.recv_bytes_from(b7, 0)
    b8  : Socket <- show_recv(r8)
    x   : Unit <- Socket.close(a8)
    y   : Unit <- Socket.close(b8)
    return 0

def go(a: Socket & U32, b: Socket & U32) -> IO(U32):
  (s1, p1) = a
  (s2, p2) = b
  run(s1, s2, p2)

def udp_retry(port: U32, r: Result<&1, &1, U32 & String, Socket>, rest: IO(Socket & U32)) ->
  IO(Socket & U32):
  match r:
    case Done{s}:
      IO.pure(Socket & U32, (s, port))
    case Fail{e}:
      rest

def udp_bind_any(tries: Nat, +port: U32) -> IO(Socket & U32):
  match tries:
    case 0n:
      IO.die(Socket & U32, 1, "no free udp port")
    case 1n+t:
      rest = udp_bind_any(t, (port + 1 : U32))
      do IO<Socket & U32>:
        r : Result<&1, &1, U32 & String, Socket> <- UDP.bind("127.0.0.1", port)
        udp_retry(port, r, rest)

def main() -> IO(U32):
  do IO<U32>:
    a : Socket & U32 <- udp_bind_any(32n, 19613)
    b : Socket & U32 <- udp_bind_any(32n, 19633)
    go(a, b)

#|0 255 195 161 128
#|195 161
#|empty
#|1 2
#|22, kept 1 256
#|22, kept 7 8
#|wait
#|ready sent
#|ready 9 200 0
#|ready 22, kept 300
#|wait
#|recv failed 22
