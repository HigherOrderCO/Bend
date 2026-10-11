# The reader of demos/io_http_fetch (copied: tests import only tests) over
# a real loopback socket: split the separator, body and UTF-8 character
# across writes, then close. At the byte limit the final character's bytes
# arrive separately; only the following EOF fits.
import Base

# the text after the first blank line; e says whether the window w,
# the last 4 characters as a big-endian word, is "\r\n\r\n" (218762506)
def http_body.go(s: String, e: Bool, w: U32) -> String:
  match s:
    case SNil{}:
      SNil{}
    case SCon{Chr{a}, t}:
      match e:
        case True{}:
          SCon{Chr{a}, t}
        case False{}:
          +w2 = (w * 256 + a : U32)
          http_body.go(t, U32.is_eq(w2, 218762506), w2)

# the body of a response: the text after the status line and headers
def http_body(s: String) -> String:
  http_body.go(s, False{}, 0)

# Base has no byte-to-text conversion. Match the TCP text codec's UTF-8
# replacement policy, including an incomplete final character.
type Utf8 is Data:
  Utf8{left: U32, code: U32, low: U32, high: U32, text: String}

def utf8_start(+b: U32, +text: String) -> Utf8:
  +left = Bool.pick(U32, U32.is_lt(b, 224), 1,
    Bool.pick(U32, U32.is_lt(b, 240), 2, 3))
  Bool.pick(Utf8, U32.is_lt(b, 128),
    Utf8{0, 0, 128, 191, SCon{Chr{b}, text}},
    Bool.pick(Utf8, Bool.or(U32.is_lt(b, 194), U32.is_gt(b, 244)),
      Utf8{0, 0, 128, 191, SCon{Chr{65533}, text}},
      Utf8{left, U32.and(b, U32.shrn(63, U32.to_nat(left))),
        Bool.pick(U32, U32.is_eq(b, 224), 160,
          Bool.pick(U32, U32.is_eq(b, 240), 144, 128)),
        Bool.pick(U32, U32.is_eq(b, 237), 159,
          Bool.pick(U32, U32.is_eq(b, 244), 143, 191)), text}))

def utf8_more(+b: U32, +left: U32, +code: U32, +text: String,
  valid: Bool) -> Utf8:
  match valid:
    case False{}:
      utf8_start(b, SCon{Chr{65533}, text})
    case True{}:
      +next = U32.sub(left, 1)
      +value = U32.or(U32.shln(code, 6n), U32.and(b, 63))
      Utf8{next, value, 128, 191,
        Bool.pick(String, U32.is_zero(next), SCon{Chr{value}, text}, text)}

def utf8_byte(state: Utf8, +b: U32) -> Utf8:
  match state:
    case Utf8{left, code, low, high, text}:
      match left:
        case 0:
          utf8_start(b, text)
        case _:
          utf8_more(b, left, code, text,
            Bool.and(U32.is_ge(b, low), U32.is_le(b, high)))

def utf8_finish(state: Utf8) -> String:
  match state:
    case Utf8{left, code, low, high, +text}:
      String.reverse(Bool.pick(String, U32.is_zero(left), text,
        SCon{Chr{65533}, text}))

def utf8(bytes: List<&2, U32>) -> String:
  utf8_finish(List.foldl(~&2, ~U32, ~Utf8, ~(state => b => utf8_byte(state, b)), bytes,
    Utf8{0, 0, 128, 191, ""}))

def fetch_fail(s: Socket, code: U32, message: String) -> IO(String):
  do IO<String>:
    Socket.close(s)
    IO.die(String, code, message)

# chunks are newest-first; foldl prepends each once without recursive concat
def fetch_done(s: Socket, chunks: List<&2, List<&2, U32>>) -> IO(String):
  do IO<String>:
    Socket.close(s)
    return utf8(List.foldl(~&2, ~List<&2, U32>, ~List<&2, U32>,
      ~(acc => chunk => List.append(&2, U32, chunk, acc)), chunks, Nil{}))

# At the exact limit, the one-byte probe must be EOF.
def fetch_end(
  m: Socket & Result<&1, &1, U32 & String, Maybe<&1, List<&2, U32>>>,
  chunks: List<&2, List<&2, U32>>
) -> IO(String):
  (s, r) = m
  match r:
    case Fail{(code, message)}:
      fetch_fail(s, code, message)
    case Done{chunk}:
      match chunk:
        case None{}:
          fetch_done(s, chunks)
        case Some{bytes}:
          fetch_fail(s, 1, "HTTP response exceeds 65536 bytes")

def fetch_read(
  fuel: Nat,
  remaining: U32,
  m: Socket & Result<&1, &1, U32 & String, Maybe<&1, List<&2, U32>>>,
  chunks: List<&2, List<&2, U32>>
) -> IO(String):
  match fuel remaining:
    case 0n _:
      fetch_end(m, chunks)
    case _ 0:
      fetch_end(m, chunks)
    case 1n+fuel2 _:
      (s, r) = m
      match r:
        case Fail{(code, message)}:
          fetch_fail(s, code, message)
        case Done{None{}}:
          fetch_done(s, chunks)
        case Done{Some{chunk}}:
          match chunk:
            case Nil{}:
              fetch_done(s, chunks)
            case +h <> +t:
              +left = U32.sub(remaining, U32.from_nat(List.length(&2, U32, chunk)))
              do IO<String>:
                next : Socket & Result<&1, &1, U32 & String, Maybe<&1, List<&2, U32>>> <-
                  TCP.recv_bytes(s, U32.max(U32.min(left, 8192), 1))
                fetch_read(fuel2, left, next, chunk <> chunks)

def ascii(text: String) -> List<&2, U32>:
  match text:
    case SNil{}:
      Nil{}
    case SCon{Chr{byte}, rest}:
      byte <> ascii(rest)

def send_done(m: Socket & Result<&1, &1, (U32 & String) & List<&2, U32>, Unit>) -> IO(Socket):
  (s, r) = m
  match r:
    case Done{u}:
      IO.pure(Socket, s)
    case Fail{((code, message), rest)}:
      do IO<Socket>:
        Socket.close(s)
        IO.die(Socket, code, message)

def serve(parts: List<&2, List<&2, U32>>, s: Socket) -> IO(Unit):
  match parts:
    case Nil{}:
      Socket.close(s)
    case part <> rest:
      do IO<Unit>:
        sent : Socket & Result<&1, &1, (U32 & String) & List<&2, U32>, Unit> <- TCP.send_bytes(s, part)
        next : Socket <- send_done(sent)
        IO.sleep(30)
        serve(rest, next)

def listen_done(port: U32, r: Result<&1, &1, U32 & String, Listener>,
  rest: IO(Listener & U32)) -> IO(Listener & U32):
  match r:
    case Done{listener}:
      IO.pure(Listener & U32, (listener, port))
    case Fail{error}:
      rest

def listen_any(tries: Nat, +port: U32) -> IO(Listener & U32):
  match tries:
    case 0n:
      IO.die(Listener & U32, 1, "no free tcp port")
    case 1n+rest:
      later = listen_any(rest, (port + 1 : U32))
      do IO<Listener & U32>:
        r : Result<&1, &1, U32 & String, Listener> <- TCP.listen("127.0.0.1", port)
        listen_done(port, r, later)

def accepted(parts: List<&2, List<&2, U32>>, client: Socket,
  m: Listener & Result<&1, &1, U32 & String, Socket>) -> IO(String):
  (listener, r) = m
  do IO<String>:
    server : Socket <- IO.pass(Socket, r)
    Listener.close(listener)
    IO.spawn(Unit, serve(parts, server))
    first : Socket & Result<&1, &1, U32 & String, Maybe<&1, List<&2, U32>>> <-
      TCP.recv_bytes(client, 8192)
    fetch_read(65536n, 65536, first, Nil{})

def connected(parts: List<&2, List<&2, U32>>, bound: Listener & U32) -> IO(String):
  (listener, port) = bound
  do IO<String>:
    client : Socket <- IO.try(Socket, TCP.connect("127.0.0.1", port))
    m : Listener & Result<&1, &1, U32 & String, Socket> <- TCP.accept(listener)
    accepted(parts, client, m)

def exchange(parts: List<&2, List<&2, U32>>) -> IO(String):
  do IO<String>:
    bound : Listener & U32 <- listen_any(32n, 19721)
    connected(parts, bound)

# The header is 19 bytes; ASCII padding plus the two bytes of é totals 65536.
# Seven 8191-byte chunks plus 8178 bytes avoid deep constructor recursion.
def limit_parts(extra: List<&2, U32>) -> List<&2, List<&2, U32>>:
  padding = List.replicate(List<&2, U32>, 7n, List.replicate(U32, 8191n, 97))
  ascii("HTTP/1.0 200 OK\r\n\r\n") <> List.append(&2, List<&2, U32>, padding,
    [List.replicate(U32, 8178n, 97), [195], List.append(&2, U32, [169], extra)])

def main() -> IO(Unit):
  do IO<Unit>:
    fragmented : String <- exchange([ascii("HTTP/1.0 200 OK\r"), ascii("\n\r"),
      ascii("\nA"), [195], [169], ascii("BC")])
    IO.print(http_body(fragmented))
    +limit : String <- exchange(limit_parts(Nil{}))
    IO.print(Nat.show(String.length(http_body(limit))))
    IO.print(String.drop(http_body(limit), 65515n))
    empty : String <- exchange(Nil{})
    IO.print(String.append("empty:", Bool.show(String.is_empty(empty))))
    malformed : String <- exchange([ascii("no header separator")])
    IO.print(String.append("malformed:", Bool.show(String.is_empty(http_body(malformed)))))

#|AéBC
#|65516
#|é
#|empty:True
#|malformed:True
