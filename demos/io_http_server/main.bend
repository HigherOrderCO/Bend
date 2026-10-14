# HTTP server: binds port 8080 and answers every connection with a
# hello world page. Each accepted socket goes to a computation of its
# own (IO.spawn) that reads the request, sends the response and
# closes; the accept loop is @unsafe: it runs until killed. Try it:
#
#   bend demos/io_http_server/main.bend -o http_server && ./http_server
#   curl http://127.0.0.1:8080
import Base

# the response for a page: the status line, the headers, a blank
# line, then the page
def http_response(page: String) -> String:
  "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nConnection: close\r\n\r\n" ++ page

def http_sent(m: Socket & Result<&1, &1, (U32 & String) & String, Unit>) -> IO(Unit):
  (s, r) = m
  Socket.close(s)

def http_reply(m: Socket & Result<&1, &1, U32 & String, Maybe<&1, String>>) -> IO(Unit):
  (s, r) = m
  match r:
    case Done{Some{req}}:
      do IO<Unit>:
        sent : Socket & Result<&1, &1, (U32 & String) & String, Unit> <- TCP.send(s, http_response("<h1>Hello, world!</h1>"))
        http_sent(sent)
    case Done{None{}}:
      Socket.close(s)
    case Fail{e}:
      Socket.close(s)

def http_talk(s: Socket) -> IO(Unit):
  do IO<Unit>:
    read : Socket & Result<&1, &1, U32 & String, Maybe<&1, String>> <- TCP.recv(s, 8192)
    http_reply(read)

def http_conn(m: Listener & Result<&1, &1, U32 & String, Socket>) -> IO(Listener):
  (l, r) = m
  do IO<Listener>:
    s : Socket <- IO.pass(Socket, r)
    IO.spawn(Unit, http_talk(s))
    return l

@unsafe
def http_serve(l: Listener) -> IO(Unit):
  do IO<Unit>:
    m  : Listener & Result<&1, &1, U32 & String, Socket> <- TCP.accept(l)
    l2 : Listener <- http_conn(m)
    http_serve(l2)

def main() -> IO(Unit):
  do IO<Unit>:
    l : Listener <- IO.try(Listener, TCP.listen("127.0.0.1", 8080))
    IO.print("serving http://127.0.0.1:8080")
    http_serve(l)
