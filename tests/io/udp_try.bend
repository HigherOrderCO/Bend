# io (io_udp_try): the try_ twins of UDP.recv_from and UDP.send_to. A
# receive with ms 0 on a quiet socket answers Wait{} at once; one bounded
# by 1000 ms answers Ready{Done{datagram}} when a spawned sender sends it
# 10 ms later; one bounded by 20 ms on a quiet socket answers Wait{}
# after its deadline. try_send_to answers Ready{Done{}}, and its datagram
# arrives. A send to a malformed address fails and hands the datagram
# back, through Ready for the try_ twin. Handles thread back OUTSIDE the
# Poll and the Result.
import Base

def show_recv(m: Socket & Poll<&1, &1, Result<&1, &1, U32 & String, String & U32 & String>, Unit>) -> IO(Socket):
  (s, r) = m
  match r:
    case Ready{Done{(host, from, data)}}:
      do IO<Socket>:
        p : Unit <- IO.print("ready " ++ data)
        return s
    case Ready{Fail{(code, message)}}:
      do IO<Socket>:
        p : Unit <- IO.print("ready " ++ message)
        return s
    case Wait{u}:
      do IO<Socket>:
        p : Unit <- IO.print("wait")
        return s

def show_send(m: Socket & Result<&1, &1, (U32 & String) & String, Unit>) -> IO(Socket):
  (s, r) = m
  match r:
    case Done{u}:
      do IO<Socket>:
        p : Unit <- IO.print("sent")
        return s
    case Fail{(error, data)}:
      (code, message) = error
      do IO<Socket>:
        p : Unit <- IO.print(message ++ ", kept " ++ data)
        return s

def show_try_send(m: Socket & Poll<&1, &1, Result<&1, &1, (U32 & String) & String, Unit>, String>) -> IO(Socket):
  (s, r) = m
  match r:
    case Ready{Done{u}}:
      do IO<Socket>:
        p : Unit <- IO.print("ready sent")
        return s
    case Ready{Fail{(error, data)}}:
      (code, message) = error
      do IO<Socket>:
        p : Unit <- IO.print("ready " ++ message ++ ", kept " ++ data)
        return s
    case Wait{rest}:
      do IO<Socket>:
        p : Unit <- IO.print("wait, kept " ++ rest)
        return s

def quiet(m: Socket & Result<&1, &1, (U32 & String) & String, Unit>) -> IO(Unit):
  (s, r) = m
  Socket.close(s)

def late(s: Socket, port: U32) -> IO(Unit):
  do IO<Unit>:
    u : Unit <- IO.sleep(10)
    m : Socket & Result<&1, &1, (U32 & String) & String, Unit> <-
      UDP.send_to(s, "127.0.0.1", port, "late")
    quiet(m)

def run(s1: Socket, s2: Socket, s3: Socket, +p2: U32) -> IO(U32):
  do IO<U32>:
    m1 : Socket & Poll<&1, &1, Result<&1, &1, U32 & String, String & U32 & String>, Unit> <-
      UDP.try_recv_from(s2, 64, 0)
    r1 : Socket <- show_recv(m1)
    u  : Unit <- IO.spawn(Unit, late(s1, p2))
    m2 : Socket & Poll<&1, &1, Result<&1, &1, U32 & String, String & U32 & String>, Unit> <-
      UDP.try_recv_from(r1, 64, 1000)
    r2 : Socket <- show_recv(m2)
    m3 : Socket & Poll<&1, &1, Result<&1, &1, U32 & String, String & U32 & String>, Unit> <-
      UDP.try_recv_from(r2, 64, 20)
    r3 : Socket <- show_recv(m3)
    m4 : Socket & Poll<&1, &1, Result<&1, &1, (U32 & String) & String, Unit>, String> <-
      UDP.try_send_to(s3, "127.0.0.1", p2, "now", 50)
    t1 : Socket <- show_try_send(m4)
    m5 : Socket & Poll<&1, &1, Result<&1, &1, U32 & String, String & U32 & String>, Unit> <-
      UDP.try_recv_from(r3, 64, 1000)
    r4 : Socket <- show_recv(m5)
    m6 : Socket & Result<&1, &1, (U32 & String) & String, Unit> <-
      UDP.send_to(t1, "1.2.3.", 9, "back")
    t2 : Socket <- show_send(m6)
    m7 : Socket & Poll<&1, &1, Result<&1, &1, (U32 & String) & String, Unit>, String> <-
      UDP.try_send_to(t2, "1.2.3.", 9, "again", 0)
    t3 : Socket <- show_try_send(m7)
    x  : Unit <- Socket.close(r4)
    y  : Unit <- Socket.close(t3)
    return 0

def go(a: Socket & U32, b: Socket & U32, c: Socket & U32) -> IO(U32):
  (s1, p1) = a
  (s2, p2) = b
  (s3, p3) = c
  run(s1, s2, s3, p2)

law udp_retry:
  for port : U32
  for r    : Result<&1, &1, (U32 & String), Socket>
  for rest : IO(Socket & U32)
  IO(Socket & U32)

def udp_retry(port, r, rest):
  match r:
    case Done{s}:
      IO.pure(Socket & U32, (s, port))
    case Fail{e}:
      rest

law udp_bind_any:
  for tries : Nat
  for +port : U32
  IO(Socket & U32)

def udp_bind_any(tries, port):
  match tries:
    case 0n:
      IO.die(Socket & U32, 1, "no free udp port")
    case 1n+t:
      rest = udp_bind_any(t, (port + 1 : U32))
      do IO<Socket & U32>:
        r : Result<&1, &1, (U32 & String), Socket> <- UDP.bind("127.0.0.1", port)
        udp_retry(port, r, rest)

def main() -> IO(U32):
  do IO<U32>:
    a : Socket & U32 <- udp_bind_any(32n, 19413)
    b : Socket & U32 <- udp_bind_any(32n, 19433)
    c : Socket & U32 <- udp_bind_any(32n, 19453)
    go(a, b, c)

#|wait
#|ready late
#|wait
#|ready sent
#|ready now
#|Invalid argument, kept back
#|ready Invalid argument, kept again
