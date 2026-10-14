# HTTP client: a GET to ip-api.com (208.95.112.1: the TCP kit has no
# DNS) that prints the body of the answer: the status, country, city
# and IP of the caller. Read until EOF (Connection: close), up to
# 65536 bytes including headers, then decode UTF-8 once. Try it:
#
#   bend demos/io_http_fetch/main.bend
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

def fetch_sent(m: Socket & Result<&1, &1, (U32 & String) & String, Unit>) -> IO(String):
  (s, r) = m
  match r:
    case Fail{((code, message), rest)}:
      fetch_fail(s, code, message)
    case Done{u}:
      do IO<String>:
        first : Socket & Result<&1, &1, U32 & String, Maybe<&1, List<&2, U32>>> <-
          TCP.recv_bytes(s, 8192)
        fetch_read(65536n, 65536, first, Nil{})

def main() -> IO(Unit):
  do IO<Unit>:
    c    : Socket <- IO.try(Socket, TCP.connect("208.95.112.1", 80))
    sent : Socket & Result<&1, &1, (U32 & String) & String, Unit> <- TCP.send(c, "GET /line/?fields=status,country,city,query HTTP/1.1\r\nHost: ip-api.com\r\nConnection: close\r\n\r\n")
    resp : String <- fetch_sent(sent)
    IO.print(http_body(resp))
