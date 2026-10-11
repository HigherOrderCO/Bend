# Types
# =====

# Data
# ----

type Empty is Data:

type Unit is Data:
  Unit{}

type Bool is Data:
  False{}
  True{}

type Cmp is Data:
  LT{}
  EQ{}
  GT{}

type Either<a, b, -A: Kind(a), -B: Kind(b)> is Kind(a <&> b):
  Inl{value: A}
  Inr{value: B}

type Sigma<a, b, -A: Kind(a), -B: @-x: A -> Kind(b)> is Kind(a <&> b):
  Tuple{fst: A, snd: B(fst)}

type Nat is Data:
  Zero{}
  Succ{pred: Nat}

type Maybe<a, -A: Kind(a)> is Kind(a):
  None{}
  Some{value: A}

type Result<a, b, -E: Kind(a), -A: Kind(b)> is Kind(a <&> b):
  Fail{error: E}
  Done{value: A}

# What a try_ operation answers: Ready{x}, x what its blocking twin answers,
# or Wait{rest} once ms milliseconds pass first (0: at once), rest what did
# not happen (the value or the bytes not sent).
type Poll<a, b, -A: Kind(a), -R: Kind(b)> is Kind(a <&> b):
  Ready{value: A}
  Wait{rest: R}

type List<a, -A: Kind(a)> is Kind(a):
  Nil{}
  Con{head: A, tail: List<a, A>}

type Word.Nil is Data:
  WNil{}

type Word.Con<-p: Nat> is Data:
  WCon{head: Bool, tail: Word(p)}

def Word(n: Nat) -> Data:
  match n:
    case 0n:
      Word.Nil
    case 1n+p:
      Word.Con<p>

type U32 is Data:
  U32{data: Word(32n)}

type F32 is Data:
  F32{data: Word(32n)}

type Char is Data:
  Chr{code: U32}

type String is Data:
  SNil{}
  SCon{head: Char, tail: String}

type Array<-T: Type> is Type:
  ALeaf{value: T}
  ANode{xs: Array<T>, ys: Array<T>}

type Image is Data:
  Pix{color: U32}
  Qua{tl: Image, tr: Image, bl: Image, br: Image}

type Event is Data:
  Key{code: U32, down: Bool}
  Mouse{x: U32, y: U32, button: U32, down: Bool}
  Move{x: U32, y: U32}
  Look{dx: F32, dy: F32}
  Scroll{x: U32, y: U32, dx: F32, dy: F32}
  Close{}

type Map<a, -V: Kind(a)> is Kind(a):
  MTip{}
  MLeaf{key: String, val: V}
  MNode{pos: Nat, lo: Map<a, V>, hi: Map<a, V>}

# Effects
# -------

# A handle is opaque, a law with no constructor and no field, so no
# program forges one, and linear (Type), so none copies or reuses one.
law File:
  Type

law Socket:
  Type

law Listener:
  Type

law Window:
  Type

law Audio:
  Type

# A channel is Data: the computations it links each hold a copy.
law Chan:
  for -A: Type
  Data

type IO.OP<-R: Type> is Type:
  Emit{value: R}
  Halt{code: U32, message: String}

type App<-S: Type> is Type:
  App{view: S -> Pair(S, Image), tick: List<Event> -> S -> IO(Maybe<S>)}

# Defs
# ====

# Pair
# ----

def Pair(-A: Type, -B: Type) -> Type:
  Sigma<&1, &1, A, _ => B>

def Exists(-A: Type, -B: @-x: A -> Type) -> Type:
  Sigma<&1, &1, A, B>

def Or(-A: Type, -B: Type) -> Type:
  Either<&1, &1, A, B>

def Pair.fst(-A: Type, -B: Type, p: A & B) -> A:
  (a, b) = p
  a

def Pair.snd(-A: Type, -B: Type, p: A & B) -> B:
  (a, b) = p
  b

# Effects
# -------

def IO(-A: Type) -> Type:
  @-R: Type ->
  @k: (A -> IO.OP<R>) ->
  IO.OP<R>

def IO.pure(-A: Type, x: A) -> IO(A):
  R => k => k(x)

def IO.bind(-A: Type, -B: Type, m: IO(A), f: A -> IO(B)) -> IO(B):
  R => k => m(R, x => f(x, R, k))

def IO.print(text: String) -> IO(Unit):
  import "./effs/print.c"
  import "./effs/print.js"

def IO.write(text: String) -> IO(Unit):
  import "./effs/write.c"
  import "./effs/write.js"

def IO.print_err(text: String) -> IO(Unit):
  import "./effs/print_err.c"
  import "./effs/print_err.js"

def IO.get_env(name: String) -> IO(Result<&1, &1, U32 & String, String>):
  import "./effs/get_env.c"
  import "./effs/get_env.js"

# The command line, less the runtime's own options: the program as invoked
# (the binary, the JS file or the .bend file), then its arguments.
def IO.args() -> IO(List<String>):
  import "./effs/args.c"
  import "./effs/args.js"

def Process.run(program: String, args: List<String>, input: String,
  max_output: U32, timeout_ms: U32) ->
  IO(Result<&1, &1, U32 & String, U32 & (String & String)>):
  import "./effs/process_run.c"
  import "./effs/process_run.js"

def IO.die(-A: Type, code: U32, msg: String) -> IO(A):
  R => k => Halt{code, msg}

def IO.pass(-A: Type, r: Result<&1, &1, U32 & String, A>) -> IO(A):
  match r:
    case Done{value}:
      IO.pure(A, value)
    case Fail{(code, message)}:
      IO.die(A, code, message)

def IO.try(-A: Type, act: IO(Result<&1, &1, U32 & String, A>)) -> IO(A):
  IO.bind(Result<&1, &1, U32 & String, A>, A, act, IO.pass(A))

def IO.random_u32() -> IO(Result<&1, &1, U32 & String, U32>):
  import "./effs/random_u32.c"
  import "./effs/random_u32.js"

def IO.spawn(-A: Type, act: IO(A)) -> IO(Unit):
  import "./effs/spawn.c"
  import "./effs/spawn.js"

def IO.sleep(ms: U32) -> IO(Unit):
  import "./effs/sleep.c"
  import "./effs/sleep.js"

def IO.now() -> IO(Nat):
  import "./effs/now.c"
  import "./effs/now.js"

def IO.thread_count() -> IO(U32):
  import "./effs/thread_count.c"
  import "./effs/thread_count.js"

def Chan.new(-A: Type, room: U32) -> IO(Chan(A)):
  import "./effs/chan.c"
  import "./effs/chan.js"

# Done{} once the channel takes the value, Fail{value} if it is closed.
def Chan.send(-A: Type, chan: Chan(A), value: A) -> IO(Result<&1, &1, A, Unit>):
  import "./effs/chan.c"
  import "./effs/chan.js"

# Some{value}, or None{} once the channel is closed and drained.
def Chan.recv(-A: Type, chan: Chan(A)) -> IO(Maybe<&1, A>):
  import "./effs/chan.c"
  import "./effs/chan.js"

def Chan.try_send(-A: Type, chan: Chan(A), value: A, ms: U32) ->
  IO(Poll<&1, &1, Result<&1, &1, A, Unit>, A>):
  import "./effs/chan.c"
  import "./effs/chan.js"

def Chan.try_recv(-A: Type, chan: Chan(A), ms: U32) ->
  IO(Poll<&1, &1, Maybe<&1, A>, Unit>):
  import "./effs/chan.c"
  import "./effs/chan.js"

def Chan.close(-A: Type, chan: Chan(A)) -> IO(Unit):
  import "./effs/chan.c"
  import "./effs/chan.js"

def IO.fork.go(-A: Type, act: IO(A), chan: Chan(A)) -> IO(Chan(A)):
  +c = chan
  sent = IO.bind(A, Result<&1, &1, A, Unit>, act, x => Chan.send(A, c, x))
  IO.bind(Unit, Chan(A), IO.spawn(Result<&1, &1, A, Unit>, sent),
    u => IO.pure(Chan(A), c))

def IO.fork(-A: Type, act: IO(A)) -> IO(Chan(A)):
  IO.bind(Chan(A), Chan(A), Chan.new(A, 1), IO.fork.go(A, act))

def IO.join.go(-A: Type, chan: Chan(A), got: Maybe<&1, A>) -> IO(A):
  match got:
    case None{}:
      IO.die(A, 1, "IO.join: the channel was closed")
    case Some{value}:
      IO.bind(Unit, A, Chan.close(A, chan), u => IO.pure(A, value))

def IO.join(-A: Type, +chan: Chan(A)) -> IO(A):
  IO.bind(Maybe<&1, A>, A, Chan.recv(A, chan), IO.join.go(A, chan))

# Some{x} if act answers x within ms milliseconds, else None{}. The loser
# is not cancelled: it runs to its end before the program exits.
def IO.within(-A: Type, ms: U32, act: IO(A)) -> IO(Maybe<&1, A>):
  won = IO.bind(A, Maybe<&1, A>, act, x => IO.pure(Maybe<&1, A>, Some{x}))
  late = IO.bind(Unit, Maybe<&1, A>, IO.sleep(ms), u => IO.pure(Maybe<&1, A>, None{}))
  both = IO.bind(Chan(Maybe<&1, A>), Chan(Maybe<&1, A>), IO.fork(Maybe<&1, A>, won),
    IO.fork.go(Maybe<&1, A>, late))
  IO.bind(Chan(Maybe<&1, A>), Maybe<&1, A>, both, c => IO.join(Maybe<&1, A>, c))

def File.open(path: String, mode: String) ->
  IO(Result<&1, &1, U32 & String, File>):
  import "./effs/file_open.c"
  import "./effs/file_open.js"

def File.read(file: File, max: U32) ->
  IO(File & Result<&1, &1, U32 & String, String>):
  import "./effs/file_read.c"
  import "./effs/file_read.js"

def File.read_bytes(file: File, max: U32) ->
  IO(File & Result<&1, &1, U32 & String, List<&2, U32>>):
  import "./effs/file_read.c"
  import "./effs/file_read.js"

def File.read_at(file: File, offset: U32, max: U32) ->
  IO(File & Result<&1, &1, U32 & String, List<&2, U32>>):
  import "./effs/file_read.c"
  import "./effs/file_read.js"

def File.size(file: File) ->
  IO(File & Result<&1, &1, U32 & String, U32>):
  import "./effs/file_size.c"
  import "./effs/file_size.js"

def File.write(file: File, data: String) ->
  IO(File & Result<&1, &1, U32 & String, Unit>):
  import "./effs/file_write.c"
  import "./effs/file_write.js"

def File.write_bytes(file: File, data: List<&2, U32>) ->
  IO(File & Result<&1, &1, U32 & String, Unit>):
  import "./effs/file_write.c"
  import "./effs/file_write.js"

def File.close(file: File) -> IO(Unit):
  import "./effs/file_close.c"
  import "./effs/file_close.js"

def TCP.listen(host: String, port: U32) -> IO(Result<&1, &1, U32 & String, Listener>):
  import "./effs/tcp_listen.c"
  import "./effs/tcp_listen.js"

def TCP.accept(listener: Listener) ->
  IO(Listener & Result<&1, &1, U32 & String, Socket>):
  import "./effs/tcp_accept.c"
  import "./effs/tcp_accept.js"

def TCP.try_accept(listener: Listener, ms: U32) ->
  IO(Listener & Poll<&1, &1, Result<&1, &1, U32 & String, Socket>, Unit>):
  import "./effs/tcp_accept.c"
  import "./effs/tcp_accept.js"

def TCP.connect(host: String, port: U32) ->
  IO(Result<&1, &1, U32 & String, Socket>):
  import "./effs/tcp_connect.c"
  import "./effs/tcp_connect.js"

# A failed send hands back what the kernel did not take. Sent means taken
# by the kernel, not yet read by the peer.
def TCP.send(sock: Socket, data: String) ->
  IO(Socket & Result<&1, &1, (U32 & String) & String, Unit>):
  import "./effs/tcp_send.c"
  import "./effs/tcp_send.js"

def TCP.try_send(sock: Socket, data: String, ms: U32) ->
  IO(Socket & Poll<&1, &1, Result<&1, &1, (U32 & String) & String, Unit>, String>):
  import "./effs/tcp_send.c"
  import "./effs/tcp_send.js"

# Some{data}, or None{} once the peer has sent everything (it may still
# read). A max of 0 fails with EINVAL, as in every receive below.
def TCP.recv(sock: Socket, max: U32) ->
  IO(Socket & Result<&1, &1, U32 & String, Maybe<&1, String>>):
  import "./effs/tcp_recv.c"
  import "./effs/tcp_recv.js"

def TCP.try_recv(sock: Socket, max: U32, ms: U32) ->
  IO(Socket & Poll<&1, &1, Result<&1, &1, U32 & String, Maybe<&1, String>>, Unit>):
  import "./effs/tcp_recv.c"
  import "./effs/tcp_recv.js"

# The bytes as they are (0..255), one List cell each: no UTF-8 decoding, so
# binary data and a character split between two reads survive. A value past
# 255 fails with EINVAL before any byte is sent.
def TCP.send_bytes(sock: Socket, data: List<&2, U32>) ->
  IO(Socket & Result<&1, &1, (U32 & String) & List<&2, U32>, Unit>):
  import "./effs/tcp_send.c"
  import "./effs/tcp_send.js"

def TCP.try_send_bytes(sock: Socket, data: List<&2, U32>, ms: U32) ->
  IO(Socket & Poll<&1, &1, Result<&1, &1, (U32 & String) & List<&2, U32>, Unit>,
    List<&2, U32>>):
  import "./effs/tcp_send.c"
  import "./effs/tcp_send.js"

def TCP.recv_bytes(sock: Socket, max: U32) ->
  IO(Socket & Result<&1, &1, U32 & String, Maybe<&1, List<&2, U32>>>):
  import "./effs/tcp_recv.c"
  import "./effs/tcp_recv.js"

def TCP.try_recv_bytes(sock: Socket, max: U32, ms: U32) ->
  IO(Socket & Poll<&1, &1, Result<&1, &1, U32 & String, Maybe<&1, List<&2, U32>>>,
    Unit>):
  import "./effs/tcp_recv.c"
  import "./effs/tcp_recv.js"

def UDP.bind(host: String, port: U32) -> IO(Result<&1, &1, U32 & String, Socket>):
  import "./effs/udp_bind.c"
  import "./effs/udp_bind.js"

# A datagram goes whole or not at all: a failed send hands it back.
def UDP.send_to(sock: Socket, host: String, port: U32, data: String) ->
  IO(Socket & Result<&1, &1, (U32 & String) & String, Unit>):
  import "./effs/udp_send_to.c"
  import "./effs/udp_send_to.js"

def UDP.try_send_to(sock: Socket, host: String, port: U32, data: String, ms: U32) ->
  IO(Socket & Poll<&1, &1, Result<&1, &1, (U32 & String) & String, Unit>, String>):
  import "./effs/udp_send_to.c"
  import "./effs/udp_send_to.js"

# The datagram's bytes as they are (0..255), one List cell each: no UTF-8
# encoding, so a binary protocol's datagram goes out as written. A value
# past 255 fails with EINVAL before anything is sent.
def UDP.send_bytes_to(sock: Socket, host: String, port: U32, data: List<&2, U32>) ->
  IO(Socket & Result<&1, &1, (U32 & String) & List<&2, U32>, Unit>):
  import "./effs/udp_send_to.c"
  import "./effs/udp_send_to.js"

def UDP.try_send_bytes_to(sock: Socket, host: String, port: U32, data: List<&2, U32>,
  ms: U32) ->
  IO(Socket & Poll<&1, &1, Result<&1, &1, (U32 & String) & List<&2, U32>, Unit>,
    List<&2, U32>>):
  import "./effs/udp_send_to.c"
  import "./effs/udp_send_to.js"

# A datagram may be empty: UDP has no end, so no None{}.
def UDP.recv_from(sock: Socket, max: U32) ->
  IO(Socket & Result<&1, &1, U32 & String, String & U32 & String>):
  import "./effs/udp_recv_from.c"
  import "./effs/udp_recv_from.js"

def UDP.try_recv_from(sock: Socket, max: U32, ms: U32) ->
  IO(Socket & Poll<&1, &1, Result<&1, &1, U32 & String, String & U32 & String>, Unit>):
  import "./effs/udp_recv_from.c"
  import "./effs/udp_recv_from.js"

# The datagram's bytes as they are: no UTF-8 decoding, so every byte of a
# binary datagram survives.
def UDP.recv_bytes_from(sock: Socket, max: U32) ->
  IO(Socket & Result<&1, &1, U32 & String, String & U32 & List<&2, U32>>):
  import "./effs/udp_recv_from.c"
  import "./effs/udp_recv_from.js"

def UDP.try_recv_bytes_from(sock: Socket, max: U32, ms: U32) ->
  IO(Socket & Poll<&1, &1, Result<&1, &1, U32 & String,
    String & U32 & List<&2, U32>>, Unit>):
  import "./effs/udp_recv_from.c"
  import "./effs/udp_recv_from.js"

def Socket.close(socket: Socket) -> IO(Unit):
  import "./effs/socket_close.c"
  import "./effs/socket_close.js"

def Listener.close(listener: Listener) -> IO(Unit):
  import "./effs/listener_close.c"
  import "./effs/listener_close.js"

def Window.open(title: String, width: U32, height: U32) ->
  IO(Result<&1, &1, U32 & String, Window>):
  import "./effs/window.c"
  import "./effs/window_open.js"

def Window.frame(window: Window, image: Image) ->
  IO(Window & Image & List<Event>):
  import "./effs/window.c"
  import "./effs/window_frame.js"

def Window.set_title(window: Window, title: String) -> IO(Window):
  import "./effs/window.c"
  import "./effs/window_set_title.js"

# While grabbed, the cursor is hidden and held, and the mouse's motion
# comes as Look deltas instead of Move; losing focus releases it.
def Window.grab(window: Window, on: Bool) -> IO(Window):
  import "./effs/window.c"
  import "./effs/window_grab.js"

def Window.close(window: Window) -> IO(Unit):
  import "./effs/window.c"
  import "./effs/window_close.js"

def Audio.open(rate: U32) -> IO(Result<&1, &1, U32 & String, Audio>):
  import "./effs/audio.c"
  import "./effs/audio_open.js"

def Audio.write(audio: Audio, samples: List<&2, F32>) -> IO(Audio & U32):
  import "./effs/audio.c"
  import "./effs/audio_write.js"

def Audio.close(audio: Audio) -> IO(Unit):
  import "./effs/audio.c"
  import "./effs/audio_close.js"

# Equal
# -----

law Equal.cong:
  for -A: Type
  for -B: Type
  for -f: A -> B
  for -a: A
  for -b: A
  for  e: {a == b : A}
  {f(a) == f(b) : B}

def Equal.cong(A, B, f, a, b, e):
  %e : {f(a) == f(_) : B}
  {==}

law Equal.sym:
  for -A: Type
  for -a: A
  for -b: A
  for  e: {a == b : A}
  {b == a : A}

def Equal.sym(A, a, b, e):
  %e : {_ == a : A}
  {==}

law Equal.trans:
  for -A : Type
  for -a : A
  for -b : A
  for -c : A
  for ab : {a == b : A}
  for bc : {b == c : A}
  {a == c : A}

def Equal.trans(A, a, b, c, ab, bc):
  %bc : {a == _ : A}
  ab

# Empty
# -----

def Empty.absurd(-A: Type, e: Empty) -> A:
  match e:

# Bool
# ----

def Bool.not(b: Bool) -> Bool:
  match b:
    case False{}:
      True{}
    case True{}:
      False{}

def Bool.and(a: Bool, b: Bool) -> Bool:
  match a:
    case False{}:
      False{}
    case True{}:
      b

def Bool.or(a: Bool, b: Bool) -> Bool:
  match a:
    case False{}:
      b
    case True{}:
      True{}

def Bool.xor(a: Bool, b: Bool) -> Bool:
  match a:
    case False{}:
      b
    case True{}:
      Bool.not(b)

def Bool.cmp(a: Bool, b: Bool) -> Cmp:
  match a b:
    case False{} False{}:
      EQ{}
    case False{} True{}:
      LT{}
    case True{} False{}:
      GT{}
    case True{} True{}:
      EQ{}

def Bool.full_add(a: Bool, +b: Bool, +c: Bool) -> Bool & Bool:
  match a:
    case False{}:
      (Bool.xor(b, c), Bool.and(b, c))
    case True{}:
      (Bool.not(Bool.xor(b, c)), Bool.or(b, c))

def Bool.pick(-A: Type, c: Bool, a: A, b: A) -> A:
  match c:
    case False{}:
      b
    case True{}:
      a

def Bool.to_u32(b: Bool) -> U32:
  match b:
    case False{}:
      0
    case True{}:
      1

# Cmp
# ---

def Cmp.is_lt(c: Cmp) -> Bool:
  match c:
    case LT{}:
      True{}
    case o:
      False{}

def Cmp.is_eq(c: Cmp) -> Bool:
  match c:
    case EQ{}:
      True{}
    case o:
      False{}

def Cmp.is_gt(c: Cmp) -> Bool:
  match c:
    case GT{}:
      True{}
    case o:
      False{}

def Cmp.is_le(c: Cmp) -> Bool:
  match c:
    case GT{}:
      False{}
    case o:
      True{}

def Cmp.is_ge(c: Cmp) -> Bool:
  match c:
    case LT{}:
      False{}
    case o:
      True{}

# Nat
# ---

def Nat.double(n: Nat) -> Nat:
  match n:
    case 0n:
      0n
    case 1n+p:
      2n+Nat.double(p)

def Nat.add(a: Nat, b: Nat) -> Nat:
  match a:
    case 0n:
      b
    case 1n+p:
      1n+Nat.add(p, b)

def Nat.sub(a: Nat, b: Nat) -> Nat:
  match a b:
    case 0n _:
      0n
    case 1n+ap 0n:
      1n+ap
    case 1n+ap 1n+bp:
      Nat.sub(ap, bp)

def Nat.mul(a: Nat, +b: Nat) -> Nat:
  match a:
    case 0n:
      0n
    case 1n+p:
      Nat.add(b, Nat.mul(p, b))

def Nat.divmod.go(n: Nat, m: Nat, d: Nat, r: Nat) -> Nat & Nat:
  match n:
    case 0n:
      (d, r)
    case 1n+np:
      match m:
        case 0n:
          Nat.divmod.go(np, r, 1n+d, 0n)
        case 1n+mp:
          Nat.divmod.go(np, mp, d, 1n+r)

def Nat.divmod(a: Nat, b: Nat) -> Nat & Nat:
  match b:
    case 0n:
      (0n, a)
    case 1n+bp:
      Nat.divmod.go(a, bp, 0n, 0n)

def Nat.cmp(a: Nat, b: Nat) -> Cmp:
  match a b:
    case 0n 0n:
      EQ{}
    case 0n 1n+bp:
      LT{}
    case 1n+ap 0n:
      GT{}
    case 1n+ap 1n+bp:
      Nat.cmp(ap, bp)

def Nat.is_lt(a: Nat, b: Nat) -> Bool:
  Cmp.is_lt(Nat.cmp(a, b))

def Nat.is_eq(a: Nat, b: Nat) -> Bool:
  Cmp.is_eq(Nat.cmp(a, b))

def Nat.is_ne(a: Nat, b: Nat) -> Bool:
  Bool.not(Cmp.is_eq(Nat.cmp(a, b)))

def Nat.is_le(a: Nat, b: Nat) -> Bool:
  Cmp.is_le(Nat.cmp(a, b))

def Nat.is_gt(a: Nat, b: Nat) -> Bool:
  Cmp.is_gt(Nat.cmp(a, b))

def Nat.is_ge(a: Nat, b: Nat) -> Bool:
  Cmp.is_ge(Nat.cmp(a, b))

def Nat.min(+a: Nat, +b: Nat) -> Nat:
  match a b:
    case 0n _:
      0n
    case 1n+ap 0n:
      0n
    case 1n+ap 1n+bp:
      1n+Nat.min(ap, bp)

def Nat.max(+a: Nat, +b: Nat) -> Nat:
  match a b:
    case 0n _:
      b
    case 1n+ap 0n:
      1n+ap
    case 1n+ap 1n+bp:
      1n+Nat.max(ap, bp)

law Nat.ge_refl:
  for a: Nat
  {Nat.is_ge(a, a) == True{} : Bool}

def Nat.ge_refl(a):
  match a:
    case 0n:
      {==}
    case 1n+p:
      Nat.ge_refl(p)

law Nat.max_ge_l:
  for +a: Nat
  for +b: Nat
  {Nat.is_ge(Nat.max(a, b), a) == True{} : Bool}

def Nat.max_ge_l(a, b):
  match a b:
    case 0n 0n:
      {==}
    case 0n 1n+bp:
      {==}
    case 1n+ap 0n:
      Nat.ge_refl(a)
    case 1n+ap 1n+bp:
      Nat.max_ge_l(ap, bp)

law Nat.max_ge_r:
  for +a: Nat
  for +b: Nat
  {Nat.is_ge(Nat.max(a, b), b) == True{} : Bool}

def Nat.max_ge_r(a, b):
  match a b:
    case 0n _:
      Nat.ge_refl(b)
    case 1n+ap 0n:
      {==}
    case 1n+ap 1n+bp:
      Nat.max_ge_r(ap, bp)

def Nat.div(a: Nat, b: Nat) -> Nat:
  Pair.fst(Nat, Nat, Nat.divmod(a, b))

def Nat.mod(a: Nat, b: Nat) -> Nat:
  Pair.snd(Nat, Nat, Nat.divmod(a, b))

def Nat.pow(+a: Nat, b: Nat) -> Nat:
  match b:
    case 0n:
      1n
    case 1n+p:
      Nat.mul(a, Nat.pow(a, p))

# Maybe
# -----

def Maybe.pure(a, -A: Kind(a), x: A) -> Maybe<a, A>:
  Some{x}

def Maybe.bind(
  a, -A: Kind(a), -B: Kind(a), m: Maybe<a, A>, f: A -> Maybe<a, B>
) -> Maybe<a, B>:
  match m:
    case None{}:
      None{}
    case Some{x}:
      f(x)

def Maybe.default(a, -A: Kind(a), m: Maybe<a, A>, d: A) -> A:
  match m:
    case None{}:
      d
    case Some{x}:
      x

def Maybe.is_some(a, -A: Kind(a), m: Maybe<a, A>) -> Bool:
  match m:
    case None{}:
      False{}
    case Some{x}:
      True{}

def Maybe.is_none(a, -A: Kind(a), m: Maybe<a, A>) -> Bool:
  Bool.not(Maybe.is_some(a, A, m))

def Maybe.map(
  a, -A: Kind(a), -B: Kind(a), f: A -> B, m: Maybe<a, A>
) -> Maybe<a, B>:
  match m:
    case None{}:
      None{}
    case Some{x}:
      Some{f(x)}

def Maybe.or(a, -A: Kind(a), m: Maybe<a, A>, n: Maybe<a, A>) -> Maybe<a, A>:
  match m:
    case None{}:
      n
    case Some{x}:
      Some{x}

# Result
# ------

def Result.pure(a, b, -E: Kind(a), -A: Kind(b), x: A) -> Result<a, b, E, A>:
  Done{x}

def Result.bind(
  a, b, -E: Kind(a), -A: Kind(b), -B: Kind(b), r: Result<a, b, E, A>,
  f: A -> Result<a, b, E, B>
) -> Result<a, b, E, B>:
  match r:
    case Fail{e}:
      Fail{e}
    case Done{x}:
      f(x)

def Result.default(
  a, b, -E: Kind(a), -A: Kind(b), r: Result<a, b, E, A>, d: A
) -> A:
  match r:
    case Fail{e}:
      d
    case Done{x}:
      x

def Result.is_done(
  a, b, -E: Kind(a), -A: Kind(b), r: Result<a, b, E, A>
) -> Bool:
  match r:
    case Fail{e}:
      False{}
    case Done{x}:
      True{}

def Result.is_fail(
  a, b, -E: Kind(a), -A: Kind(b), r: Result<a, b, E, A>
) -> Bool:
  Bool.not(Result.is_done(a, b, E, A, r))

def Result.map(
  a, b, -E: Kind(a), -A: Kind(b), -B: Kind(b), f: A -> B,
  r: Result<a, b, E, A>
) -> Result<a, b, E, B>:
  match r:
    case Fail{e}:
      Fail{e}
    case Done{x}:
      Done{f(x)}

# List
# ----

def List.map(~A: Type, ~B: Type, ~f: A -> B, xs: List<A>) -> List<B>:
  match xs:
    case Nil{}:
      Nil{}
    case h <> t:
      f(h) <> List.map(~A, ~B, ~f, t)

def List.length(a, -A: Kind(a), xs: List<a, A>) -> Nat:
  match xs:
    case Nil{}:
      0n
    case h <> t:
      1n+List.length(a, A, t)

def List.append(a, -A: Kind(a), xs: List<a, A>, ys: List<a, A>) -> List<a, A>:
  match xs:
    case Nil{}:
      ys
    case h <> t:
      h <> List.append(a, A, t, ys)

def List.concat(a, -A: Kind(a), xss: List<a, List<a, A>>) -> List<a, A>:
  match xss:
    case Nil{}:
      Nil{}
    case h <> t:
      List.append(a, A, h, List.concat(a, A, t))

def List.reverse.go(a, -A: Kind(a), xs: List<a, A>, acc: List<a, A>) ->
  List<a, A>:
  match xs:
    case Nil{}:
      acc
    case h <> t:
      List.reverse.go(a, A, t, h <> acc)

def List.reverse(a, -A: Kind(a), xs: List<a, A>) -> List<a, A>:
  List.reverse.go(a, A, xs, Nil{})

def List.is_empty(a, -A: Kind(a), xs: List<a, A>) -> Bool:
  match xs:
    case Nil{}:
      True{}
    case h <> t:
      False{}

def List.head(a, -A: Kind(a), xs: List<a, A>) -> Maybe<a, A>:
  match xs:
    case Nil{}:
      None{}
    case h <> t:
      Some{h}

def List.tail(a, -A: Kind(a), xs: List<a, A>) -> List<a, A>:
  match xs:
    case Nil{}:
      Nil{}
    case h <> t:
      t

def List.last.go(a, -A: Kind(a), xs: List<a, A>, last: A) -> A:
  match xs:
    case Nil{}:
      last
    case h <> t:
      List.last.go(a, A, t, h)

def List.last(a, -A: Kind(a), xs: List<a, A>) -> Maybe<a, A>:
  match xs:
    case Nil{}:
      None{}
    case h <> t:
      Some{List.last.go(a, A, t, h)}

def List.get(a, -A: Kind(a), xs: List<a, A>, n: Nat) -> Maybe<a, A>:
  match xs n:
    case Nil{} _:
      None{}
    case h <> t 0n:
      Some{h}
    case h <> t 1n+p:
      List.get(a, A, t, p)

def List.set(a, -A: Kind(a), xs: List<a, A>, n: Nat, x: A) -> List<a, A>:
  match xs n:
    case Nil{} _:
      Nil{}
    case h <> t 0n:
      x <> t
    case h <> t 1n+p:
      h <> List.set(a, A, t, p, x)

def List.take(a, -A: Kind(a), xs: List<a, A>, n: Nat) -> List<a, A>:
  match xs n:
    case Nil{} _:
      Nil{}
    case h <> t 0n:
      Nil{}
    case h <> t 1n+p:
      h <> List.take(a, A, t, p)

def List.drop(a, -A: Kind(a), xs: List<a, A>, n: Nat) -> List<a, A>:
  match xs n:
    case Nil{} _:
      Nil{}
    case h <> t 0n:
      h <> t
    case h <> t 1n+p:
      List.drop(a, A, t, p)

def List.zip(
  a, -A: Kind(a), b, -B: Kind(b), xs: List<a, A>, ys: List<b, B>
) -> List<&1, A & B>:
  match xs ys:
    case Nil{} _:
      Nil{}
    case h <> t Nil{}:
      Nil{}
    case x <> xt y <> yt:
      (x, y) <> List.zip(a, A, b, B, xt, yt)

def List.range.go(+n: Nat, acc: List<&2, Nat>) -> List<&2, Nat>:
  match n:
    case 0n:
      acc
    case 1n+p:
      List.range.go(p, p <> acc)

def List.range(n: Nat) -> List<&2, Nat>:
  List.range.go(n, Nil{})

def List.replicate(-A: Data, n: Nat, +x: A) -> List<&2, A>:
  match n:
    case 0n:
      Nil{}
    case 1n+p:
      x <> List.replicate(A, p, x)

def List.filter.put(-A: Data, h: A, r: List<&2, A>, keep: Bool) -> List<&2, A>:
  match keep:
    case False{}:
      r
    case True{}:
      h <> r

def List.filter(~A: Data, ~f: A -> Bool, xs: List<&2, A>) -> List<&2, A>:
  match xs:
    case Nil{}:
      Nil{}
    case +h <> t:
      List.filter.put(A, h, List.filter(~A, ~f, t), f(h))

def List.foldl(
  ~a: Quant, ~A: Kind(a), ~B: Type, ~f: B -> A -> B, xs: List<a, A>, acc: B
) -> B:
  match xs:
    case Nil{}:
      acc
    case h <> t:
      List.foldl(~a, ~A, ~B, ~f, t, f(acc, h))

def List.foldr(
  ~a: Quant, ~A: Kind(a), ~B: Type, ~f: A -> B -> B, xs: List<a, A>, z: B
) -> B:
  match xs:
    case Nil{}:
      z
    case h <> t:
      f(h, List.foldr(~a, ~A, ~B, ~f, t, z))

def List.any(~a: Quant, ~A: Kind(a), ~f: A -> Bool, xs: List<a, A>) -> Bool:
  match xs:
    case Nil{}:
      False{}
    case h <> t:
      Bool.or(f(h), List.any(~a, ~A, ~f, t))

def List.all(~a: Quant, ~A: Kind(a), ~f: A -> Bool, xs: List<a, A>) -> Bool:
  match xs:
    case Nil{}:
      True{}
    case h <> t:
      Bool.and(f(h), List.all(~a, ~A, ~f, t))

def List.find.put(-A: Data, h: A, r: Maybe<&2, A>, hit: Bool) -> Maybe<&2, A>:
  match hit:
    case False{}:
      r
    case True{}:
      Some{h}

def List.find(~A: Data, ~f: A -> Bool, xs: List<&2, A>) -> Maybe<&2, A>:
  match xs:
    case Nil{}:
      None{}
    case +h <> t:
      List.find.put(A, h, List.find(~A, ~f, t), f(h))

def List.contains(
  ~A: Data, ~eq: A -> A -> Bool, xs: List<&2, A>, +x: A
) -> Bool:
  match xs:
    case Nil{}:
      False{}
    case h <> t:
      Bool.or(eq(h, x), List.contains(~A, ~eq, t, x))

def List.merge.step(
  -A: Data, acc: List<&2, A>, x: A, xt: List<&2, A>, y: A, yt: List<&2, A>,
  le: Bool
) -> List<&2, A> & List<&2, A> & List<&2, A>:
  match le:
    case False{}:
      (y <> acc, x <> xt, yt)
    case True{}:
      (x <> acc, xt, y <> yt)

# The fuel bounds the steps: a sort passes its length, enough for any merge.
def List.merge.go(
  ~A: Data, ~le: A -> A -> Bool, fuel: Nat,
  st: List<&2, A> & List<&2, A> & List<&2, A>
) -> List<&2, A>:
  match fuel:
    case 0n:
      (acc, xs, ys) = st
      List.reverse.go(&2, A, acc, List.append(&2, A, xs, ys))
    case 1n+f:
      (acc, xs, ys) = st
      match xs ys:
        case Nil{} Nil{}:
          List.reverse.go(&2, A, acc, Nil{})
        case Nil{} y <> yt:
          List.reverse.go(&2, A, acc, y <> yt)
        case x <> xt Nil{}:
          List.reverse.go(&2, A, acc, x <> xt)
        case +x <> xt +y <> yt:
          List.merge.go(~A, ~le, f,
            List.merge.step(A, acc, x, xt, y, yt, le(x, y)))

def List.sort.runs(-A: Data, xs: List<&2, A>) -> List<&2, List<&2, A>>:
  match xs:
    case Nil{}:
      Nil{}
    case h <> t:
      [h] <> List.sort.runs(A, t)

def List.sort.pass(
  ~A: Data, ~le: A -> A -> Bool, +n: Nat, runs: List<&2, List<&2, A>>
) -> List<&2, List<&2, A>>:
  match runs:
    case Nil{}:
      Nil{}
    case r <> Nil{}:
      [r]
    case r1 <> r2 <> rest:
      List.merge.go(~A, ~le, n, (Nil{}, r1, r2))
        <> List.sort.pass(~A, ~le, n, rest)

def List.sort.go(
  ~A: Data, ~le: A -> A -> Bool, fuel: Nat, +n: Nat, runs: List<&2, List<&2, A>>
) -> List<&2, A>:
  match fuel:
    case 0n:
      List.concat(&2, A, runs)
    case 1n+f:
      match runs:
        case Nil{}:
          Nil{}
        case r <> Nil{}:
          r
        case r1 <> r2 <> rest:
          List.sort.go(~A, ~le, f, n,
            List.sort.pass(~A, ~le, n, r1 <> r2 <> rest))

def List.sort(~A: Data, ~le: A -> A -> Bool, xs: List<&2, A>) -> List<&2, A>:
  match xs:
    case Nil{}:
      Nil{}
    case h <> t:
      +ys = {h <> t : List<&2, A>}
      +n = List.length(&2, A, ys)
      List.sort.go(~A, ~le, n, n, List.sort.runs(A, ys))

def List.for_each(
  ~a: Quant, ~A: Kind(a), ~f: A -> IO(Unit), xs: List<a, A>
) -> IO(Unit):
  match xs:
    case Nil{}:
      IO.pure(Unit, Unit{})
    case h <> t:
      IO.bind(Unit, Unit, f(h), u => List.for_each(~a, ~A, ~f, t))

# Word
# ----

def Word.zero(n: Nat) -> Word(n):
  match n:
    case 0n:
      WNil{}
    case 1n+p:
      WCon{False{}, Word.zero(p)}

def Word.not(n: Nat, w: Word(n)) -> Word(n):
  match n:
    case 0n:
      WNil{}
    case 1n+p:
      match w:
        case WCon{b, t}:
          WCon{Bool.not(b), Word.not(p, t)}

def Word.and(n: Nat, a: Word(n), b: Word(n)) -> Word(n):
  match n:
    case 0n:
      WNil{}
    case 1n+p:
      match a b:
        case WCon{ab, at} WCon{bb, bt}:
          WCon{Bool.and(ab, bb), Word.and(p, at, bt)}

def Word.or(n: Nat, a: Word(n), b: Word(n)) -> Word(n):
  match n:
    case 0n:
      WNil{}
    case 1n+p:
      match a b:
        case WCon{ab, at} WCon{bb, bt}:
          WCon{Bool.or(ab, bb), Word.or(p, at, bt)}

def Word.xor(n: Nat, a: Word(n), b: Word(n)) -> Word(n):
  match n:
    case 0n:
      WNil{}
    case 1n+p:
      match a b:
        case WCon{ab, at} WCon{bb, bt}:
          WCon{Bool.xor(ab, bb), Word.xor(p, at, bt)}

def Word.shl.put(n: Nat, c: Bool, w: Word(n)) -> Word(n):
  match n:
    case 0n:
      WNil{}
    case 1n+p:
      match w:
        case WCon{b, t}:
          WCon{c, Word.shl.put(p, b, t)}

def Word.shl(n: Nat, w: Word(n)) -> Word(n):
  match n:
    case 0n:
      WNil{}
    case 1n+p:
      match w:
        case WCon{b, t}:
          WCon{False{}, Word.shl.put(p, b, t)}

def Word.shl.out.con(-p: Nat, c: Bool, r: Bool & Word(p)) -> Bool & Word(1n+p):
  (hi, t2) = r
  (hi, WCon{c, t2})

def Word.shl.out(n: Nat, c: Bool, w: Word(n)) -> Bool & Word(n):
  match n:
    case 0n:
      (c, WNil{})
    case 1n+p:
      match w:
        case WCon{b, t}:
          Word.shl.out.con(p, c, Word.shl.out(p, b, t))

def Word.shr.pad(n: Nat, w: Word(n)) -> Word(1n+n):
  match n:
    case 0n:
      WCon{False{}, w}
    case 1n+p:
      match w:
        case WCon{b, t}:
          WCon{b, Word.shr.pad(p, t)}

def Word.shr(n: Nat, w: Word(n)) -> Word(n):
  match n:
    case 0n:
      WNil{}
    case 1n+p:
      match w:
        case WCon{b, t}:
          Word.shr.pad(p, t)

def Word.cmp.fin(ab: Bool, bb: Bool, t: Cmp) -> Cmp:
  match t:
    case LT{}:
      LT{}
    case EQ{}:
      Bool.cmp(ab, bb)
    case GT{}:
      GT{}

def Word.cmp(n: Nat, a: Word(n), b: Word(n)) -> Cmp:
  match n:
    case 0n:
      EQ{}
    case 1n+p:
      match a b:
        case WCon{ab, at} WCon{bb, bt}:
          Word.cmp.fin(ab, bb, Word.cmp(p, at, bt))

def Word.inc(n: Nat, w: Word(n)) -> Word(n):
  match n:
    case 0n:
      WNil{}
    case 1n+p:
      match w:
        case WCon{False{}, t}:
          WCon{True{}, t}
        case WCon{True{}, t}:
          WCon{False{}, Word.inc(p, t)}

law Word.adc:
  for n: Nat
  for a: Word(n)
  for b: Word(n)
  for f: Bool
  for c: Bool
  Word(n)

def Word.adc.con(p: Nat, at: Word(p), bt: Word(p), f: Bool, sk: Bool & Bool) ->
  Word(1n+p):
  (s, k) = sk
  WCon{s, Word.adc(p, at, bt, f, k)}

def Word.adc(n, a, b, f, c):
  match n:
    case 0n:
      WNil{}
    case 1n+p:
      match a b f:
        case WCon{ab, at} WCon{bb, bt} False{}:
          Word.adc.con(p, at, bt, False{}, Bool.full_add(ab, bb, c))
        case WCon{ab, at} WCon{bb, bt} True{}:
          Word.adc.con(p, at, bt, True{}, Bool.full_add(ab, Bool.not(bb), c))

def Word.add(n: Nat, a: Word(n), b: Word(n)) -> Word(n):
  Word.adc(n, a, b, False{}, False{})

def Word.sub(n: Nat, a: Word(n), b: Word(n)) -> Word(n):
  Word.adc(n, a, b, True{}, True{})

law Word.add_comm.arm:
  for p  : Nat
  for at : Word(p)
  for bt : Word(p)
  for k  : Bool
  for -h : Bool
  {WCon{h, Word.adc(p, at, bt, False{}, k)}
    == WCon{h, Word.adc(p, bt, at, False{}, k)} : Word.Con<p>}

law Word.add_comm.go:
  for n: Nat
  for a: Word(n)
  for b: Word(n)
  for c: Bool
  {Word.adc(n, a, b, False{}, c) == Word.adc(n, b, a, False{}, c) : Word(n)}

def Word.add_comm.arm(p, at, bt, k, h):
  Equal.cong(Word(p), Word(1n+p), w => WCon{h, w},
    Word.adc(p, at, bt, False{}, k), Word.adc(p, bt, at, False{}, k),
    Word.add_comm.go(p, at, bt, k))

def Word.add_comm.go(n, a, b, c):
  match n:
    case 0n:
      match a b:
        case WNil{} WNil{}:
          {==}
    case 1n+p:
      match a b c:
        case WCon{False{}, at} WCon{False{}, bt} False{}:
          Word.add_comm.arm(p, at, bt, False{}, False{})
        case WCon{False{}, at} WCon{False{}, bt} True{}:
          Word.add_comm.arm(p, at, bt, False{}, True{})
        case WCon{False{}, at} WCon{True{}, bt} False{}:
          Word.add_comm.arm(p, at, bt, False{}, True{})
        case WCon{False{}, at} WCon{True{}, bt} True{}:
          Word.add_comm.arm(p, at, bt, True{}, False{})
        case WCon{True{}, at} WCon{False{}, bt} False{}:
          Word.add_comm.arm(p, at, bt, False{}, True{})
        case WCon{True{}, at} WCon{False{}, bt} True{}:
          Word.add_comm.arm(p, at, bt, True{}, False{})
        case WCon{True{}, at} WCon{True{}, bt} False{}:
          Word.add_comm.arm(p, at, bt, True{}, False{})
        case WCon{True{}, at} WCon{True{}, bt} True{}:
          Word.add_comm.arm(p, at, bt, True{}, True{})

law Word.add_comm:
  for n: Nat
  for a: Word(n)
  for b: Word(n)
  {Word.add(n, a, b) == Word.add(n, b, a) : Word(n)}

def Word.add_comm(n, a, b):
  Word.add_comm.go(n, a, b, False{})

def Word.to_nat(n: Nat, w: Word(n)) -> Nat:
  match n:
    case 0n:
      0n
    case 1n+p:
      match w:
        case WCon{False{}, t}:
          Nat.double(Word.to_nat(p, t))
        case WCon{True{}, t}:
          1n+Nat.double(Word.to_nat(p, t))

def Word.mul.go(+n: Nat, m: Nat, a: Word(m), +b: Word(n), acc: Word(n)) ->
  Word(n):
  match m:
    case 0n:
      acc
    case 1n+mp:
      match a:
        case WCon{False{}, at}:
          Word.mul.go(n, mp, at, Word.shl(n, b), acc)
        case WCon{True{}, at}:
          Word.mul.go(n, mp, at, Word.shl(n, b), Word.add(n, acc, b))

def Word.mul(+n: Nat, a: Word(n), b: Word(n)) -> Word(n):
  Word.mul.go(n, n, a, b, Word.zero(n))

# U32
# ---

def U32.inc(a: U32) -> U32:
  match a:
    case U32{x}:
      U32{Word.inc(32n, x)}

def U32.add(a: U32, b: U32) -> U32:
  match a b:
    case U32{x} U32{y}:
      U32{Word.add(32n, x, y)}

law U32.add_comm:
  for a: U32
  for b: U32
  {U32.add(a, b) == U32.add(b, a) : U32}

def U32.add_comm(a, b):
  match a b:
    case U32{x} U32{y}:
      Equal.cong(Word(32n), U32, w => U32{w}, Word.add(32n, x, y),
        Word.add(32n, y, x), Word.add_comm(32n, x, y))

def U32.sub(a: U32, b: U32) -> U32:
  match a b:
    case U32{x} U32{y}:
      U32{Word.sub(32n, x, y)}

def U32.mul(a: U32, b: U32) -> U32:
  match a b:
    case U32{x} U32{y}:
      U32{Word.mul(32n, x, y)}

def U32.not(a: U32) -> U32:
  match a:
    case U32{x}:
      U32{Word.not(32n, x)}

def U32.and(a: U32, b: U32) -> U32:
  match a b:
    case U32{x} U32{y}:
      U32{Word.and(32n, x, y)}

def U32.or(a: U32, b: U32) -> U32:
  match a b:
    case U32{x} U32{y}:
      U32{Word.or(32n, x, y)}

def U32.xor(a: U32, b: U32) -> U32:
  match a b:
    case U32{x} U32{y}:
      U32{Word.xor(32n, x, y)}

def U32.shl(a: U32) -> U32:
  match a:
    case U32{x}:
      U32{Word.shl(32n, x)}

def U32.shr(a: U32) -> U32:
  match a:
    case U32{x}:
      U32{Word.shr(32n, x)}

def U32.shln(a: U32, n: Nat) -> U32:
  match n:
    case 0n:
      a
    case 1n+p:
      U32.shl(U32.shln(a, p))

def U32.shrn(a: U32, n: Nat) -> U32:
  match n:
    case 0n:
      a
    case 1n+p:
      U32.shr(U32.shrn(a, p))

def U32.cmp(a: U32, b: U32) -> Cmp:
  match a b:
    case U32{x} U32{y}:
      Word.cmp(32n, x, y)

def U32.is_eq(a: U32, b: U32) -> Bool:
  Cmp.is_eq(U32.cmp(a, b))

def U32.is_ne(a: U32, b: U32) -> Bool:
  Bool.not(Cmp.is_eq(U32.cmp(a, b)))

def U32.is_lt(a: U32, b: U32) -> Bool:
  Cmp.is_lt(U32.cmp(a, b))

def U32.is_le(a: U32, b: U32) -> Bool:
  Cmp.is_le(U32.cmp(a, b))

def U32.is_gt(a: U32, b: U32) -> Bool:
  Cmp.is_gt(U32.cmp(a, b))

def U32.is_ge(a: U32, b: U32) -> Bool:
  Cmp.is_ge(U32.cmp(a, b))

def U32.is_zero(a: U32) -> Bool:
  U32.is_eq(a, 0)

def U32.to_nat(a: U32) -> Nat:
  match a:
    case U32{w}:
      Word.to_nat(32n, w)

def U32.from_nat(n: Nat) -> U32:
  match n:
    case 0n:
      0
    case 1n+p:
      U32.inc(U32.from_nat(p))

def U32.log2.go(+k: Nat, +n: U32, b: Nat) -> Nat:
  match k:
    case 0n:
      b
    case 1n+p:
      +s = U32.to_nat(U32.shln(1, p))
      +t = Nat.mul(s, U32.to_nat(Bool.to_u32(U32.is_ne(U32.shrn(n, s), 0))))
      U32.log2.go(p, U32.shrn(n, t), Nat.add(b, t))

def U32.log2(n: U32) -> Nat:
  U32.log2.go(5n, n, 0n)

def U32.divmod.go.fin(-p: Nat, q: Word(p), s: U32, b: U32, g: Bool) ->
  Word(1n+p) & U32:
  match g:
    case True{}:
      (WCon{True{}, q}, U32.sub(s, b))
    case False{}:
      (WCon{False{}, q}, s)

def U32.divmod.go.shl(-p: Nat, q: Word(p), +b: U32, ts: Bool & Word(32n)) ->
  Word(1n+p) & U32:
  (t, s) = ts
  +s2 = {U32{s} : U32}
  U32.divmod.go.fin(p, q, s2, b, Bool.or(t, U32.is_ge(s2, b)))

def U32.divmod.go.rec(-p: Nat, a0: Bool, +b: U32, qr: Word(p) & U32) ->
  Word(1n+p) & U32:
  (q, r) = qr
  U32{rw} = r
  U32.divmod.go.shl(p, q, b, Word.shl.out(32n, a0, rw))

def U32.divmod.go(m: Nat, a: Word(m), +b: U32) -> Word(m) & U32:
  match m a:
    case 0n WNil{}:
      (WNil{}, 0)
    case 1n+p WCon{a0, hi}:
      U32.divmod.go.rec(p, a0, b, U32.divmod.go(p, hi, b))

def U32.div.fin(qr: Word(32n) & U32) -> U32:
  (q, r) = qr
  U32{q}

def U32.div.if(aw: Word(32n), b: U32, z: Bool) -> U32:
  match z:
    case True{}:
      0
    case False{}:
      U32.div.fin(U32.divmod.go(32n, aw, b))

def U32.div(a: U32, b: U32) -> U32:
  U32{aw} = a
  U32{+bw} = b
  U32.div.if(aw, U32{bw}, U32.is_zero(U32{bw}))

def U32.mod.fin(qr: Word(32n) & U32) -> U32:
  (q, r) = qr
  r

def U32.mod.if(aw: Word(32n), b: U32, z: Bool) -> U32:
  match z:
    case True{}:
      U32{aw}
    case False{}:
      U32.mod.fin(U32.divmod.go(32n, aw, b))

def U32.mod(a: U32, b: U32) -> U32:
  U32{aw} = a
  U32{+bw} = b
  U32.mod.if(aw, U32{bw}, U32.is_zero(U32{bw}))

def U32.min(a: U32, b: U32) -> U32:
  match a b:
    case U32{+x} U32{+y}:
      Bool.pick(U32, U32.is_lt(U32{x}, U32{y}), U32{x}, U32{y})

def U32.max(a: U32, b: U32) -> U32:
  match a b:
    case U32{+x} U32{+y}:
      Bool.pick(U32, U32.is_lt(U32{x}, U32{y}), U32{y}, U32{x})

def U32.clamp(x: U32, lo: U32, hi: U32) -> U32:
  U32.min(U32.max(x, lo), hi)

def U32.pow(+a: U32, n: Nat) -> U32:
  match n:
    case 0n:
      1
    case 1n+p:
      U32.mul(a, U32.pow(a, p))

def U32.is_even(a: U32) -> Bool:
  U32.is_zero(U32.and(a, 1))

# F32
# ---

law U32.to_f32:
  for a: U32
  F32

law F32.to_u32:
  for a: F32
  U32

law F32.add:
  for a: F32
  for b: F32
  F32

law F32.sub:
  for a: F32
  for b: F32
  F32

law F32.mul:
  for a: F32
  for b: F32
  F32

law F32.div:
  for a: F32
  for b: F32
  F32

law F32.mod:
  for a: F32
  for b: F32
  F32

law F32.pow:
  for a: F32
  for b: F32
  F32

law F32.atan2:
  for a: F32
  for b: F32
  F32

law F32.neg:
  for a: F32
  F32

law F32.abs:
  for a: F32
  F32

law F32.sqrt:
  for a: F32
  F32

law F32.exp:
  for a: F32
  F32

law F32.log:
  for a: F32
  F32

law F32.log2:
  for a: F32
  F32

law F32.log10:
  for a: F32
  F32

law F32.sin:
  for a: F32
  F32

law F32.cos:
  for a: F32
  F32

law F32.tan:
  for a: F32
  F32

law F32.asin:
  for a: F32
  F32

law F32.acos:
  for a: F32
  F32

law F32.atan:
  for a: F32
  F32

law F32.sinh:
  for a: F32
  F32

law F32.cosh:
  for a: F32
  F32

law F32.tanh:
  for a: F32
  F32

law F32.floor:
  for a: F32
  F32

law F32.ceil:
  for a: F32
  F32

law F32.trunc:
  for a: F32
  F32

law F32.is_eq:
  for a: F32
  for b: F32
  Bool

law F32.is_ne:
  for a: F32
  for b: F32
  Bool

law F32.is_lt:
  for a: F32
  for b: F32
  Bool

law F32.is_le:
  for a: F32
  for b: F32
  Bool

law F32.is_gt:
  for a: F32
  for b: F32
  Bool

law F32.is_ge:
  for a: F32
  for b: F32
  Bool

law F32.show:
  for +a: F32
  String

law F32.bits:
  for a: F32
  U32

law F32.read:
  for s: String
  Maybe<&2, F32>

def F32.min(a: F32, b: F32) -> F32:
  match a b:
    case F32{+x} F32{+y}:
      Bool.pick(F32, F32.is_lt(F32{x}, F32{y}), F32{x}, F32{y})

def F32.max(a: F32, b: F32) -> F32:
  match a b:
    case F32{+x} F32{+y}:
      Bool.pick(F32, F32.is_lt(F32{x}, F32{y}), F32{y}, F32{x})

def F32.clamp(x: F32, lo: F32, hi: F32) -> F32:
  F32.min(F32.max(x, lo), hi)

def F32.lerp(a: F32, b: F32, t: F32) -> F32:
  match a:
    case F32{+x}:
      F32.add(F32{x}, F32.mul(F32.sub(b, F32{x}), t))

def F32.square(a: F32) -> F32:
  match a:
    case F32{+x}:
      F32.mul(F32{x}, F32{x})

def F32.hypot(a: F32, b: F32) -> F32:
  match a b:
    case F32{+x} F32{+y}:
      F32.sqrt(F32.add(F32.mul(F32{x}, F32{x}), F32.mul(F32{y}, F32{y})))

def F32.round(a: F32) -> F32:
  F32.floor(F32.add(a, 0.5))

def F32.pi() -> F32:
  3.14159265

def F32.from_nat(n: Nat) -> F32:
  U32.to_f32(U32.from_nat(n))

def F32.to_nat(a: F32) -> Nat:
  U32.to_nat(F32.to_u32(a))

# Char
# ----

def Char.cmp(a: Char, b: Char) -> (Char & Char) & Cmp:
  match a b:
    case Chr{+x} Chr{+y}:
      ((Chr{x}, Chr{y}), U32.cmp(x, y))

def Char.to_u32(c: Char) -> U32:
  match c:
    case Chr{x}:
      x

def Char.from_u32(x: U32) -> Char:
  Chr{x}

def Char.is_eq(a: Char, b: Char) -> Bool:
  match a b:
    case Chr{x} Chr{y}:
      U32.is_eq(x, y)

def Char.is_digit(c: Char) -> Bool:
  match c:
    case Chr{+x}:
      Bool.and(U32.is_ge(x, 48), U32.is_le(x, 57))

def Char.is_upper(c: Char) -> Bool:
  match c:
    case Chr{+x}:
      Bool.and(U32.is_ge(x, 65), U32.is_le(x, 90))

def Char.is_lower(c: Char) -> Bool:
  match c:
    case Chr{+x}:
      Bool.and(U32.is_ge(x, 97), U32.is_le(x, 122))

def Char.is_alpha(+c: Char) -> Bool:
  Bool.or(Char.is_upper(c), Char.is_lower(c))

def Char.is_space(c: Char) -> Bool:
  match c:
    case Chr{+x}:
      Bool.or(U32.is_eq(x, 32), Bool.and(U32.is_ge(x, 9), U32.is_le(x, 13)))

def Char.to_upper(c: Char) -> Char:
  match c:
    case Chr{+x}:
      Chr{U32.sub(x, U32.mul(Bool.to_u32(Char.is_lower(Chr{x})), 32))}

def Char.to_lower(c: Char) -> Char:
  match c:
    case Chr{+x}:
      Chr{U32.add(x, U32.mul(Bool.to_u32(Char.is_upper(Chr{x})), 32))}

# String
# ------

def String.append(a: String, b: String) -> String:
  match a:
    case SNil{}:
      b
    case SCon{h, t}:
      SCon{h, String.append(t, b)}

def String.cmp.rec(h1b: Char, h2b: Char, rr: (String & String) & Cmp) ->
  (String & String) & Cmp:
  ((t1b, t2b), r) = rr
  ((SCon{h1b, t1b}, SCon{h2b, t2b}), r)

law String.cmp:
  for a: String
  for b: String
  (String & String) & Cmp

def String.cmp.fin(t1: String, t2: String, hc: (Char & Char) & Cmp) ->
  (String & String) & Cmp:
  ((h1b, h2b), c) = hc
  match c:
    case LT{}:
      ((SCon{h1b, t1}, SCon{h2b, t2}), LT{})
    case EQ{}:
      String.cmp.rec(h1b, h2b, String.cmp(t1, t2))
    case GT{}:
      ((SCon{h1b, t1}, SCon{h2b, t2}), GT{})

def String.cmp(a, b):
  match a b:
    case SNil{} SNil{}:
      ((SNil{}, SNil{}), EQ{})
    case SNil{} SCon{h, t}:
      ((SNil{}, SCon{h, t}), LT{})
    case SCon{h, t} SNil{}:
      ((SCon{h, t}, SNil{}), GT{})
    case SCon{h1, t1} SCon{h2, t2}:
      String.cmp.fin(t1, t2, Char.cmp(h1, h2))

def String.eq(a: String, b: String) -> Bool:
  Cmp.is_eq(String.order(a, b))

def String.length(s: String) -> Nat:
  match s:
    case SNil{}:
      0n
    case SCon{h, t}:
      1n+String.length(t)

def String.is_empty(s: String) -> Bool:
  match s:
    case SNil{}:
      True{}
    case SCon{h, t}:
      False{}

def String.reverse.go(s: String, acc: String) -> String:
  match s:
    case SNil{}:
      acc
    case SCon{h, t}:
      String.reverse.go(t, SCon{h, acc})

def String.reverse(s: String) -> String:
  String.reverse.go(s, SNil{})

def String.order(a: String, b: String) -> Cmp:
  Pair.snd(String & String, Cmp, String.cmp(a, b))

def String.is_lt(a: String, b: String) -> Bool:
  Cmp.is_lt(String.order(a, b))

def String.is_le(a: String, b: String) -> Bool:
  Cmp.is_le(String.order(a, b))

def String.is_gt(a: String, b: String) -> Bool:
  Cmp.is_gt(String.order(a, b))

def String.is_ge(a: String, b: String) -> Bool:
  Cmp.is_ge(String.order(a, b))

law String.starts_with:
  for s: String
  for p: String
  Bool

def String.starts_with.if(t: String, pt: String, same: Bool) -> Bool:
  match same:
    case False{}:
      False{}
    case True{}:
      String.starts_with(t, pt)

def String.starts_with(s, p):
  match s p:
    case SNil{} SNil{}:
      True{}
    case SNil{} SCon{h, t}:
      False{}
    case SCon{h, t} SNil{}:
      True{}
    case SCon{x, xt} SCon{y, yt}:
      String.starts_with.if(xt, yt, Char.is_eq(x, y))

def String.ends_with(s: String, p: String) -> Bool:
  String.starts_with(String.reverse(s), String.reverse(p))

law String.contains:
  for +s: String
  for +p: String
  Bool

def String.contains.if(t: String, p: String, here: Bool) -> Bool:
  match here:
    case False{}:
      String.contains(t, p)
    case True{}:
      True{}

def String.contains(s, p):
  match s:
    case SNil{}:
      String.is_empty(p)
    case SCon{h, t}:
      String.contains.if(t, p, String.starts_with(SCon{h, t}, p))

def String.take(s: String, n: Nat) -> String:
  match s n:
    case SNil{} _:
      SNil{}
    case SCon{h, t} 0n:
      SNil{}
    case SCon{h, t} 1n+p:
      SCon{h, String.take(t, p)}

def String.drop(s: String, n: Nat) -> String:
  match s n:
    case SNil{} _:
      SNil{}
    case SCon{h, t} 0n:
      SCon{h, t}
    case SCon{h, t} 1n+p:
      String.drop(t, p)

def String.get(s: String, n: Nat) -> Maybe<&2, Char>:
  match s n:
    case SNil{} _:
      None{}
    case SCon{h, t} 0n:
      Some{h}
    case SCon{h, t} 1n+p:
      String.get(t, p)

def String.to_list(s: String) -> List<&2, Char>:
  match s:
    case SNil{}:
      Nil{}
    case SCon{h, t}:
      h <> String.to_list(t)

def String.from_list(cs: List<&2, Char>) -> String:
  match cs:
    case Nil{}:
      SNil{}
    case h <> t:
      SCon{h, String.from_list(t)}

def String.concat(xs: List<&2, String>) -> String:
  match xs:
    case Nil{}:
      SNil{}
    case h <> t:
      String.append(h, String.concat(t))

def String.join.go(xs: List<&2, String>, h: String, +sep: String) -> String:
  match xs:
    case Nil{}:
      h
    case h2 <> t:
      String.append(h, String.append(sep, String.join.go(t, h2, sep)))

def String.join(xs: List<&2, String>, +sep: String) -> String:
  match xs:
    case Nil{}:
      SNil{}
    case h <> t:
      String.join.go(t, h, sep)

def String.split.push(c: Char, ps: List<&2, String>) -> List<&2, String>:
  match ps:
    case Nil{}:
      [SCon{c, SNil{}}]
    case h <> t:
      SCon{c, h} <> t

def String.split.fin(c: Char, r: List<&2, String>, cut: Bool) ->
  List<&2, String>:
  match cut:
    case False{}:
      String.split.push(c, r)
    case True{}:
      SNil{} <> r

def String.split(s: String, +sep: Char) -> List<&2, String>:
  match s:
    case SNil{}:
      [SNil{}]
    case SCon{+h, t}:
      String.split.fin(h, String.split(t, sep), Char.is_eq(h, sep))

def String.lines(s: String) -> List<&2, String>:
  String.split(s, '\n')

def String.repeat(+s: String, n: Nat) -> String:
  match n:
    case 0n:
      SNil{}
    case 1n+p:
      String.append(s, String.repeat(s, p))

def String.to_upper(s: String) -> String:
  match s:
    case SNil{}:
      SNil{}
    case SCon{h, t}:
      SCon{Char.to_upper(h), String.to_upper(t)}

def String.to_lower(s: String) -> String:
  match s:
    case SNil{}:
      SNil{}
    case SCon{h, t}:
      SCon{Char.to_lower(h), String.to_lower(t)}

law String.trim_start:
  for s: String
  String

def String.trim_start.if(t: String, h: Char, space: Bool) -> String:
  match space:
    case False{}:
      SCon{h, t}
    case True{}:
      String.trim_start(t)

def String.trim_start(s):
  match s:
    case SNil{}:
      SNil{}
    case SCon{+h, t}:
      String.trim_start.if(t, h, Char.is_space(h))

def String.trim_end(s: String) -> String:
  String.reverse(String.trim_start(String.reverse(s)))

def String.trim(s: String) -> String:
  String.trim_end(String.trim_start(s))

# Text
# ----

def Nat.show.put(qr: Nat & Nat) -> Char & Nat:
  (q, r) = qr
  (Chr{U32.from_nat(Nat.add(48n, r))}, q)

law Nat.show.go:
  for f   : Nat
  for n   : Nat
  for acc : String
  String

def Nat.show.fin(g: Nat, acc: String, dq: Char & Nat) -> String:
  (d, q) = dq
  match q:
    case 0n:
      SCon{d, acc}
    case 1n+p:
      Nat.show.go(g, 1n+p, SCon{d, acc})

def Nat.show.go(f, n, acc):
  match f:
    case 0n:
      acc
    case 1n+g:
      Nat.show.fin(g, acc, Nat.show.put(Nat.divmod(n, 10n)))

def Nat.show(n: Nat) -> String:
  +m = n
  Nat.show.fin(m, SNil{}, Nat.show.put(Nat.divmod(m, 10n)))

law Nat.read.go:
  for s   : String
  for +acc : Nat
  Maybe<&2, Nat>

def Nat.read.if(t: String, acc: Nat, d: U32, ok: Bool) -> Maybe<&2, Nat>:
  match ok:
    case True{}:
      Nat.read.go(t, Nat.add(Nat.mul(acc, 10n), U32.to_nat(d)))
    case False{}:
      None{}

def Nat.read.go(s, acc):
  match s:
    case SNil{}:
      Some{acc}
    case SCon{Chr{x}, t}:
      +d = U32.sub(x, 48)
      Nat.read.if(t, acc, d, U32.is_lt(d, 10))

# 2^48-1 is the text 281474976710655. Past leading zeros, fewer than
# fifteen characters fit, more do not, and fifteen compare as text;
# Nat.read.go still refuses a non-digit. The gate is a def, not a
# Bool.pick: an argument is built before the call, and reading a string
# that does not fit would build a Nat past the bound. A law that reads
# "7" never builds the unary nat (each digit once divided it).
def Nat.read.trim(s: String) -> String:
  match s:
    case SCon{'0', t}:
      Nat.read.trim(t)
    case t:
      t

def Nat.read.fits(+s: String) -> Bool:
  +n = String.length(s)
  Bool.or(Nat.is_lt(n, 15n),
    Bool.and(Nat.is_eq(n, 15n), String.is_le(s, "281474976710655")))

def Nat.read.gate(+s: String, ok: Bool) -> Maybe<&2, Nat>:
  match ok:
    case True{}:
      Nat.read.go(s, 0n)
    case False{}:
      None{}

def Nat.read(s: String) -> Maybe<&2, Nat>:
  match s:
    case SNil{}:
      None{}
    case SCon{h, t}:
      +u = Nat.read.trim(SCon{h, t})
      Nat.read.gate(u, Nat.read.fits(u))

law U32.show.go:
  for f   : Nat
  for +n  : U32
  for acc : String
  String

def U32.show.fin(g: Nat, acc: String, +n: U32, z: Bool) -> String:
  match z:
    case True{}:
      acc
    case False{}:
      U32.show.go(g, U32.div(n, 10),
        SCon{Chr{U32.add(48, U32.mod(n, 10))}, acc})

def U32.show.go(f, n, acc):
  match f:
    case 0n:
      acc
    case 1n+g:
      U32.show.fin(g, acc, n, U32.is_zero(n))

def U32.show.if(a: U32, z: Bool) -> String:
  match z:
    case True{}:
      SCon{Chr{48}, SNil{}}
    case False{}:
      U32.show.go(10n, a, SNil{})

def U32.show(a: U32) -> String:
  +b = a
  U32.show.if(b, U32.is_zero(b))

law U32.read.go:
  for s   : String
  for +acc : U32
  Maybe<&2, U32>

def U32.read.if(t: String, n: U32, ok: Bool) -> Maybe<&2, U32>:
  match ok:
    case True{}:
      U32.read.go(t, n)
    case False{}:
      None{}

def U32.read.go(s, acc):
  match s:
    case SNil{}:
      Some{acc}
    case SCon{Chr{x}, t}:
      +n = U32.add(U32.mul(acc, 10), U32.sub(x, 48))
      U32.read.if(t, n, U32.is_eq(U32.div(n, 10), acc))

def U32.read(s: String) -> Maybe<&2, U32>:
  match s:
    case SNil{}:
      None{}
    case SCon{h, t}:
      U32.read.go(SCon{h, t}, 0)

def Bool.show(b: Bool) -> String:
  match b:
    case False{}:
      "False"
    case True{}:
      "True"

def Char.show(c: Char) -> String:
  SCon{c, SNil{}}

def Maybe.show(~a: Quant, ~A: Kind(a), ~f: A -> String, m: Maybe<a, A>) ->
  String:
  match m:
    case None{}:
      "None"
    case Some{x}:
      "Some(" ++ f(x) ++ ")"

def List.show.go(~a: Quant, ~A: Kind(a), ~f: A -> String, xs: List<a, A>) ->
  String:
  match xs:
    case Nil{}:
      "]"
    case h <> t:
      ", " ++ f(h) ++ List.show.go(~a, ~A, ~f, t)

def List.show(~a: Quant, ~A: Kind(a), ~f: A -> String, xs: List<a, A>) ->
  String:
  match xs:
    case Nil{}:
      "[]"
    case h <> t:
      "[" ++ f(h) ++ List.show.go(~a, ~A, ~f, t)

# Array
# -----

def Array.size.node(-T: Type, ys: Array<T>, r: Array<T> & U32) ->
  Array<T> & U32:
  (xs2, n) = r
  (ANode{xs2, ys}, U32.shl(n))

def Array.size(-T: Type, a: Array<T>) -> Array<T> & U32:
  match a:
    case ALeaf{x}:
      (ALeaf{x}, 1)
    case ANode{xs, ys}:
      Array.size.node(T, ys, Array.size(T, xs))

def Array.swap.lo(-T: Type, ys: Array<T>, r: Array<T> & T) -> Array<T> & T:
  (nxs, old) = r
  (ANode{nxs, ys}, old)

def Array.swap.hi(-T: Type, xs: Array<T>, r: Array<T> & T) -> Array<T> & T:
  (nys, old) = r
  (ANode{xs, nys}, old)

# the recursion descends into a subtree, and z says which: the caller
# computes it from the index and the subtree's half size
def Array.swap.go(-T: Type, a: Array<T>, n: U32, +i: U32, v: T, z: Bool) -> Array<T> & T:
  match a z:
    case ALeaf{x} _:
      (ALeaf{v}, x)
    case ANode{xs, ys} True{}:
      +h = U32.shr(n)
      Array.swap.lo(T, ys, Array.swap.go(T, xs, h, i, v, U32.is_lt(i, U32.shr(h))))
    case ANode{xs, ys} False{}:
      +h = U32.shr(n)
      +j = U32.sub(i, h)
      Array.swap.hi(T, xs, Array.swap.go(T, ys, h, j, v, U32.is_lt(j, U32.shr(h))))

def Array.swap.at(-T: Type, i: U32, v: T, an: Array<T> & U32) -> Array<T> & T:
  (a, +n) = an
  +j = U32.and(i, U32.sub(n, 1))
  Array.swap.go(T, a, n, j, v, U32.is_lt(j, U32.shr(n)))

def Array.swap(-T: Type, a: Array<T>, +i: U32, v: T) -> Array<T> & T:
  Array.swap.at(T, i, v, Array.size(T, a))

def Array.clone.node(
  -T: Data, cx: Array<T> & Array<T>, cy: Array<T> & Array<T>
) -> Array<T> & Array<T>:
  (xa, xb) = cx
  (ya, yb) = cy
  (ANode{xa, ya}, ANode{xb, yb})

def Array.clone(-T: Data, a: Array<T>) -> Array<T> & Array<T>:
  match a:
    case ALeaf{+x}:
      (ALeaf{x}, ALeaf{x})
    case ANode{xs, ys}:
      Array.clone.node(T, Array.clone(T, xs), Array.clone(T, ys))

# A fork is two handles to one array in O(1). Both read and write the
# same cells, so racing writes go through the atomics; a match on a
# handle copies its part, as a clone does. An atomic takes an Array<U32>
# (fadd an Array<F32>) and returns the array with the old value.

@unsafe
def Array.fork(-T: Type, +a: Array<T>) -> Array<T> & Array<T>:
  (a, a)

@unsafe
def Array.join(-T: Type, a: Array<T>, b: Array<T>) -> Array<T>:
  a

law Array.atomic.add:
  for  a: Array<U32>
  for +i: U32
  for +v: U32
  Array<U32> & U32

law Array.atomic.min:
  for  a: Array<U32>
  for +i: U32
  for +v: U32
  Array<U32> & U32

law Array.atomic.max:
  for  a: Array<U32>
  for +i: U32
  for +v: U32
  Array<U32> & U32

law Array.atomic.and:
  for  a: Array<U32>
  for +i: U32
  for +v: U32
  Array<U32> & U32

law Array.atomic.or:
  for  a: Array<U32>
  for +i: U32
  for +v: U32
  Array<U32> & U32

law Array.atomic.xor:
  for  a: Array<U32>
  for +i: U32
  for +v: U32
  Array<U32> & U32

law Array.atomic.exch:
  for  a: Array<U32>
  for +i: U32
  for +v: U32
  Array<U32> & U32

law Array.atomic.cas:
  for  a: Array<U32>
  for +i: U32
  for +x: U32
  for +v: U32
  Array<U32> & U32

law Array.atomic.fadd:
  for  a: Array<F32>
  for +i: U32
  for +v: F32
  Array<F32> & F32

def Array.new(-T: Data, +d: Nat, +v: T) -> Array<T>:
  match d:
    case 0n:
      ALeaf{v}
    case 1n+p:
      ANode{[v : T^p], [v : T^p]}

def Array.set.fin(-T: Type, r: Array<T> & T) -> Array<T>:
  (na, old) = r
  na

def Array.set(-T: Type, a: Array<T>, i: U32, v: T) -> Array<T>:
  Array.set.fin(T, Array.swap(T, a, i, v))

def Array.get.go(-T: Data, a: Array<T>, n: U32, +i: U32, z: Bool) -> Array<T> & T:
  match a z:
    case ALeaf{+x} _:
      (ALeaf{x}, x)
    case ANode{xs, ys} True{}:
      +h = U32.shr(n)
      Array.swap.lo(T, ys, Array.get.go(T, xs, h, i, U32.is_lt(i, U32.shr(h))))
    case ANode{xs, ys} False{}:
      +h = U32.shr(n)
      +j = U32.sub(i, h)
      Array.swap.hi(T, xs, Array.get.go(T, ys, h, j, U32.is_lt(j, U32.shr(h))))

def Array.get.at(-T: Data, i: U32, an: Array<T> & U32) -> Array<T> & T:
  (a, +n) = an
  +j = U32.and(i, U32.sub(n, 1))
  Array.get.go(T, a, n, j, U32.is_lt(j, U32.shr(n)))

def Array.get(-T: Data, a: Array<T>, +i: U32) -> Array<T> & T:
  Array.get.at(T, i, Array.size(T, a))

def Array.to_list.go(~T: Type, a: Array<T>, acc: List<T>) -> List<T>:
  match a:
    case ALeaf{x}:
      x <> acc
    case ANode{xs, ys}:
      Array.to_list.go(~T, xs, Array.to_list.go(~T, ys, acc))

def Array.to_list(~T: Type, a: Array<T>) -> List<T>:
  Array.to_list.go(~T, a, Nil{})

def Array.map.go(
  ~T: Data, ~U: Data, ~f: T -> U, +n: Nat, +i: U32, b: Array<U>,
  r: Array<T> & T
) -> Array<U>:
  match n:
    case 0n:
      b
    case 1n+p:
      (a, x) = r
      +j = U32.inc(i)
      Array.map.go(~T, ~U, ~f, p, j, Array.set(U, b, i, f(x)),
        Array.get(T, a, j))

def Array.map.at(
  ~T: Data, ~U: Data, ~f: T -> U, v: U, an: Array<T> & U32
) -> Array<U>:
  (a, +n) = an
  Array.map.go(~T, ~U, ~f, U32.to_nat(U32.sub(n, 1)), 1,
    Array.new(U, U32.log2(n), v), Array.get(T, a, 1))

def Array.map.seed(~T: Data, ~U: Data, ~f: T -> U, r: Array<T> & T) -> Array<U>:
  (a, x) = r
  Array.map.at(~T, ~U, ~f, f(x), Array.size(T, a))

def Array.map(~T: Data, ~U: Data, ~f: T -> U, a: Array<T>) -> Array<U>:
  Array.map.seed(~T, ~U, ~f, Array.get(T, a, 0))

# Map
# ---

def Map.bit.u(x: U32, k: Nat) -> Bool:
  U32.is_ne(U32.and(U32.shrn(x, k), 1), 0)

def Map.bit.chr(c: Char, off: Nat) -> Char & Bool:
  match c off:
    case Chr{x} 0n:
      (Chr{x}, True{})
    case Chr{+x} 1n+b:
      (Chr{x}, Map.bit.u(x, Nat.sub(31n, b)))

def Map.bit.go.chr(t: String, r: Char & Bool) -> String & Bool:
  (c2, b) = r
  (SCon{c2, t}, b)

def Map.bit.go.rec(c: Char, r: String & Bool) -> String & Bool:
  (t2, b) = r
  (SCon{c, t2}, b)

def Map.bit.go(key: String, ci: Nat, off: Nat) -> String & Bool:
  match key:
    case SNil{}:
      (SNil{}, False{})
    case SCon{c, t}:
      match ci:
        case 0n:
          Map.bit.go.chr(t, Map.bit.chr(c, off))
        case 1n+j:
          Map.bit.go.rec(c, Map.bit.go(t, j, off))

def Map.bit.at(key: String, co: Nat & Nat) -> String & Bool:
  (ci, off) = co
  Map.bit.go(key, ci, off)

def Map.bit(key: String, pos: Nat) -> String & Bool:
  Map.bit.at(key, Nat.divmod(pos, 33n))

law Map.msb.u:
  for n: Nat
  for +x: U32
  Nat

def Map.msb.u.if(p: Nat, x2: U32, z: Bool) -> Nat:
  match z:
    case True{}:
      0n
    case False{}:
      Nat.add(1n, Map.msb.u(p, U32.shr(x2)))

def Map.msb.u(n, x):
  match n:
    case 0n:
      0n
    case 1n+p:
      Map.msb.u.if(p, x, U32.is_zero(x))

def Map.diff.chr(x: U32) -> Nat:
  Nat.sub(33n, Map.msb.u(32n, x))

def Map.diff.step(x: Char, y: Char) -> Nat & Bool:
  match x y:
    case Chr{+cx} Chr{+cy}:
      (Map.diff.chr(U32.xor(cx, cy)), U32.is_eq(cx, cy))

law Map.diff:
  for a: String
  for b: String
  Nat

def Map.diff.fin(xt: String, yt: String, rc: Nat & Bool) -> Nat:
  (r, c) = rc
  match c:
    case True{}:
      Nat.add(33n, Map.diff(xt, yt))
    case False{}:
      r

def Map.diff(a, b):
  match a b:
    case SNil{} _:
      0n
    case SCon{h, t} SNil{}:
      0n
    case SCon{x, xt} SCon{y, yt}:
      Map.diff.fin(xt, yt, Map.diff.step(x, y))

def Map.new(a, -V: Kind(a)) -> Map<a, V>:
  MTip{}

# Map.put.go, Map.ins.go, Map.seek.go, Map.has.go, Map.get.go and
# Map.pop.go take the node they are in, whole, and the key's bit at it:
# the child they descend into is that node's field, a node rebuilt
def Map.put.go(-a: Quant, -V: Kind(a), node: Map<a, V>, x: V, kb: String & Bool) -> Map<a, V>:
  match node kb:
    case MNode{p, MTip{}, hi} Tuple{key, False{}}:
      MNode{p, MLeaf{key, x}, hi}
    case MNode{p, MLeaf{k, v}, hi} Tuple{key, False{}}:
      MNode{p, MLeaf{k, x}, hi}
    case MNode{p, MNode{+q, c, d}, hi} Tuple{key, False{}}:
      MNode{p, Map.put.go(a, V, MNode{q, c, d}, x, Map.bit(key, q)), hi}
    case MNode{p, lo, MTip{}} Tuple{key, True{}}:
      MNode{p, lo, MLeaf{key, x}}
    case MNode{p, lo, MLeaf{k, v}} Tuple{key, True{}}:
      MNode{p, lo, MLeaf{k, x}}
    case MNode{p, lo, MNode{+q, c, d}} Tuple{key, True{}}:
      MNode{p, lo, Map.put.go(a, V, MNode{q, c, d}, x, Map.bit(key, q))}
    case m _:
      m

def Map.put(-a: Quant, -V: Kind(a), m: Map<a, V>, key: String, x: V) -> Map<a, V>:
  match m:
    case MTip{}:
      MLeaf{key, x}
    case MLeaf{k, v}:
      MLeaf{k, x}
    case MNode{+pos, lo, hi}:
      Map.put.go(a, V, MNode{pos, lo, hi}, x, Map.bit(key, pos))

def Map.ins.splice.bit(
  a, -V: Kind(a), x: V, rest: Map<a, V>, pb: Nat, kb: String & Bool
) -> Map<a, V>:
  (key2, b) = kb
  match b:
    case False{}:
      MNode{pb, MLeaf{key2, x}, rest}
    case True{}:
      MNode{pb, rest, MLeaf{key2, x}}

def Map.ins.splice(
  a, -V: Kind(a), +p: Nat, key: String, x: V, rest: Map<a, V>
) -> Map<a, V>:
  Map.ins.splice.bit(a, V, x, rest, p, Map.bit(key, p))

# t says whether the key splits deeper than node: else it goes above
def Map.ins.go(
  -a: Quant, -V: Kind(a), node: Map<a, V>, x: V, +p: Nat, kb: String & Bool,
  t: Bool
) -> Map<a, V>:
  match node kb t:
    case MNode{q, lo, hi} Tuple{key, b} False{}:
      Map.ins.splice(a, V, p, key, x, MNode{q, lo, hi})
    case MNode{q, MTip{}, hi} Tuple{key, False{}} True{}:
      MNode{q, MLeaf{key, x}, hi}
    case MNode{q, MLeaf{k, v}, hi} Tuple{key, False{}} True{}:
      MNode{q, Map.ins.splice(a, V, p, key, x, MLeaf{k, v}), hi}
    case MNode{q, MNode{+r, c, d}, hi} Tuple{key, False{}} True{}:
      MNode{q, Map.ins.go(a, V, MNode{r, c, d}, x, p, Map.bit(key, r), Nat.is_lt(r, p)), hi}
    case MNode{q, lo, MTip{}} Tuple{key, True{}} True{}:
      MNode{q, lo, MLeaf{key, x}}
    case MNode{q, lo, MLeaf{k, v}} Tuple{key, True{}} True{}:
      MNode{q, lo, Map.ins.splice(a, V, p, key, x, MLeaf{k, v})}
    case MNode{q, lo, MNode{+r, c, d}} Tuple{key, True{}} True{}:
      MNode{q, lo, Map.ins.go(a, V, MNode{r, c, d}, x, p, Map.bit(key, r), Nat.is_lt(r, p))}
    case m _ _:
      m

def Map.ins(
  -a: Quant, -V: Kind(a), m: Map<a, V>, key: String, x: V, +p: Nat
) -> Map<a, V>:
  match m:
    case MTip{}:
      MLeaf{key, x}
    case MLeaf{k, v}:
      Map.ins.splice(a, V, p, key, x, MLeaf{k, v})
    case MNode{+pos, lo, hi}:
      Map.ins.go(a, V, MNode{pos, lo, hi}, x, p, Map.bit(key, pos), Nat.is_lt(pos, p))

def Map.lo(
  a, -V: Kind(a), -R: Type, p2: Nat, hi: Map<a, V>, r0: Map<a, V> & R
) -> Map<a, V> & R:
  (lo2, r) = r0
  (MNode{p2, lo2, hi}, r)

def Map.hi(
  a, -V: Kind(a), -R: Type, p2: Nat, lo: Map<a, V>, r0: Map<a, V> & R
) -> Map<a, V> & R:
  (hi2, r) = r0
  (MNode{p2, lo, hi2}, r)

def Map.seek.go(-a: Quant, -V: Kind(a), node: Map<a, V>, kb: String & Bool) ->
  Map<a, V> & String & Maybe<&2, String>:
  match node kb:
    case MNode{p, MTip{}, hi} Tuple{key, False{}}:
      (MNode{p, MTip{}, hi}, key, None{})
    case MNode{p, MLeaf{+k, v}, hi} Tuple{key, False{}}:
      (MNode{p, MLeaf{k, v}, hi}, key, Some{k})
    case MNode{p, MNode{+q, c, d}, hi} Tuple{key, False{}}:
      Map.lo(a, V, String & Maybe<&2, String>, p, hi, Map.seek.go(a, V, MNode{q, c, d}, Map.bit(key, q)))
    case MNode{p, lo, MTip{}} Tuple{key, True{}}:
      (MNode{p, lo, MTip{}}, key, None{})
    case MNode{p, lo, MLeaf{+k, v}} Tuple{key, True{}}:
      (MNode{p, lo, MLeaf{k, v}}, key, Some{k})
    case MNode{p, lo, MNode{+q, c, d}} Tuple{key, True{}}:
      Map.hi(a, V, String & Maybe<&2, String>, p, lo, Map.seek.go(a, V, MNode{q, c, d}, Map.bit(key, q)))
    case m Tuple{key, b}:
      (m, key, None{})

def Map.seek(-a: Quant, -V: Kind(a), m: Map<a, V>, key: String) ->
  Map<a, V> & String & Maybe<&2, String>:
  match m:
    case MTip{}:
      (MTip{}, key, None{})
    case MLeaf{+k, v}:
      (MLeaf{k, v}, key, Some{k})
    case MNode{+pos, lo, hi}:
      Map.seek.go(a, V, MNode{pos, lo, hi}, Map.bit(key, pos))

def Map.set.fin.go(
  a, -V: Kind(a), m: Map<a, V>, key: String, x: V, r: (String & String) & Cmp
) -> Map<a, V>:
  ((keyb2, k2), c) = r
  match c:
    case EQ{}:
      Map.put(a, V, m, key, x)
    case o:
      Map.ins(a, V, m, key, x, Map.diff(keyb2, k2))

def Map.set.fin(
  a, -V: Kind(a), m: Map<a, V>, key: String, x: V, keyb: String, k: String
) -> Map<a, V>:
  Map.set.fin.go(a, V, m, key, x, String.cmp(keyb, k))

def Map.set.go(
  a, -V: Kind(a), x: V, r: Map<a, V> & String & Maybe<&2, String>
) -> Map<a, V>:
  (m2, key2, found) = r
  match found:
    case None{}:
      MLeaf{key2, x}
    case Some{k}:
      +ka = {key2 : String}
      Map.set.fin(a, V, m2, ka, x, ka, k)

def Map.set(a, -V: Kind(a), m: Map<a, V>, key: String, x: V) -> Map<a, V>:
  Map.set.go(a, V, x, Map.seek(a, V, m, key))

def Map.has.leaf(a, -V: Kind(a), v: V, r: (String & String) & Cmp) ->
  Map<a, V> & Bool:
  ((key2, k2), c) = r
  (MLeaf{k2, v}, Cmp.is_eq(c))

def Map.has.go(-a: Quant, -V: Kind(a), node: Map<a, V>, kb: String & Bool) ->
  Map<a, V> & Bool:
  match node kb:
    case MNode{p, MTip{}, hi} Tuple{key, False{}}:
      (MNode{p, MTip{}, hi}, False{})
    case MNode{p, MLeaf{k, v}, hi} Tuple{key, False{}}:
      Map.lo(a, V, Bool, p, hi, Map.has.leaf(a, V, v, String.cmp(key, k)))
    case MNode{p, MNode{+q, c, d}, hi} Tuple{key, False{}}:
      Map.lo(a, V, Bool, p, hi, Map.has.go(a, V, MNode{q, c, d}, Map.bit(key, q)))
    case MNode{p, lo, MTip{}} Tuple{key, True{}}:
      (MNode{p, lo, MTip{}}, False{})
    case MNode{p, lo, MLeaf{k, v}} Tuple{key, True{}}:
      Map.hi(a, V, Bool, p, lo, Map.has.leaf(a, V, v, String.cmp(key, k)))
    case MNode{p, lo, MNode{+q, c, d}} Tuple{key, True{}}:
      Map.hi(a, V, Bool, p, lo, Map.has.go(a, V, MNode{q, c, d}, Map.bit(key, q)))
    case m _:
      (m, False{})

def Map.has(-a: Quant, -V: Kind(a), m: Map<a, V>, key: String) -> Map<a, V> & Bool:
  match m:
    case MTip{}:
      (MTip{}, False{})
    case MLeaf{k, v}:
      Map.has.leaf(a, V, v, String.cmp(key, k))
    case MNode{+pos, lo, hi}:
      Map.has.go(a, V, MNode{pos, lo, hi}, Map.bit(key, pos))

def Map.get.leaf(-V: Data, d: V, +v: V, r: (String & String) & Cmp) ->
  Map<&2, V> & V:
  ((key2, k2), c) = r
  match c:
    case EQ{}:
      (MLeaf{k2, v}, v)
    case o:
      (MLeaf{k2, v}, d)

def Map.get.go(-V: Data, node: Map<&2, V>, d: V, kb: String & Bool) -> Map<&2, V> & V:
  match node kb:
    case MNode{p, MTip{}, hi} Tuple{key2, False{}}:
      (MNode{p, MTip{}, hi}, d)
    case MNode{p, MLeaf{k, v}, hi} Tuple{key2, False{}}:
      Map.lo(&2, V, V, p, hi, Map.get.leaf(V, d, v, String.cmp(key2, k)))
    case MNode{p, MNode{+q, a, b}, hi} Tuple{key2, False{}}:
      Map.lo(&2, V, V, p, hi, Map.get.go(V, MNode{q, a, b}, d, Map.bit(key2, q)))
    case MNode{p, lo, MTip{}} Tuple{key2, True{}}:
      (MNode{p, lo, MTip{}}, d)
    case MNode{p, lo, MLeaf{k, v}} Tuple{key2, True{}}:
      Map.hi(&2, V, V, p, lo, Map.get.leaf(V, d, v, String.cmp(key2, k)))
    case MNode{p, lo, MNode{+q, a, b}} Tuple{key2, True{}}:
      Map.hi(&2, V, V, p, lo, Map.get.go(V, MNode{q, a, b}, d, Map.bit(key2, q)))
    case m _:
      (m, d)

def Map.get(-V: Data, d: V, m: Map<&2, V>, key: String) -> Map<&2, V> & V:
  match m:
    case MTip{}:
      (MTip{}, d)
    case MLeaf{k, v}:
      Map.get.leaf(V, d, v, String.cmp(key, k))
    case MNode{+pos, lo, hi}:
      Map.get.go(V, MNode{pos, lo, hi}, d, Map.bit(key, pos))

def Map.pop.lo(
  a, -V: Kind(a), pos: Nat, hi: Map<a, V>, r0: Map<a, V> & Maybe<a, V>
) -> Map<a, V> & Maybe<a, V>:
  (lo, r) = r0
  match lo:
    case MTip{}:
      (hi, r)
    case lo2:
      (MNode{pos, lo2, hi}, r)

def Map.pop.hi(
  a, -V: Kind(a), pos: Nat, lo: Map<a, V>, r0: Map<a, V> & Maybe<a, V>
) -> Map<a, V> & Maybe<a, V>:
  (hi, r) = r0
  match hi:
    case MTip{}:
      (lo, r)
    case hi2:
      (MNode{pos, lo, hi2}, r)

def Map.pop.leaf(a, -V: Kind(a), v: V, r: (String & String) & Cmp) ->
  Map<a, V> & Maybe<a, V>:
  ((key2, k2), c) = r
  match c:
    case EQ{}:
      (MTip{}, Some{v})
    case o:
      (MLeaf{k2, v}, None{})

def Map.pop.go(-a: Quant, -V: Kind(a), node: Map<a, V>, kb: String & Bool) ->
  Map<a, V> & Maybe<a, V>:
  match node kb:
    case MNode{p, MTip{}, hi} Tuple{key, False{}}:
      (hi, None{})
    case MNode{p, MLeaf{k, v}, hi} Tuple{key, False{}}:
      Map.pop.lo(a, V, p, hi, Map.pop.leaf(a, V, v, String.cmp(key, k)))
    case MNode{p, MNode{+q, c, d}, hi} Tuple{key, False{}}:
      Map.pop.lo(a, V, p, hi, Map.pop.go(a, V, MNode{q, c, d}, Map.bit(key, q)))
    case MNode{p, lo, MTip{}} Tuple{key, True{}}:
      (lo, None{})
    case MNode{p, lo, MLeaf{k, v}} Tuple{key, True{}}:
      Map.pop.hi(a, V, p, lo, Map.pop.leaf(a, V, v, String.cmp(key, k)))
    case MNode{p, lo, MNode{+q, c, d}} Tuple{key, True{}}:
      Map.pop.hi(a, V, p, lo, Map.pop.go(a, V, MNode{q, c, d}, Map.bit(key, q)))
    case m _:
      (m, None{})

def Map.pop(-a: Quant, -V: Kind(a), m: Map<a, V>, key: String) -> Map<a, V> & Maybe<a, V>:
  match m:
    case MTip{}:
      (MTip{}, None{})
    case MLeaf{k, v}:
      Map.pop.leaf(a, V, v, String.cmp(key, k))
    case MNode{+pos, lo, hi}:
      Map.pop.go(a, V, MNode{pos, lo, hi}, Map.bit(key, pos))

def Map.del.fin(a, -V: Kind(a), r: Map<a, V> & Maybe<a, V>) -> Map<a, V>:
  (m2, x) = r
  m2

def Map.del(a, -V: Kind(a), m: Map<a, V>, key: String) -> Map<a, V>:
  Map.del.fin(a, V, Map.pop(a, V, m, key))

def Map.to_list.go(
  a, -V: Kind(a), m: Map<a, V>, acc: List<a, Sigma<&2, a, String, _ => V>>
) -> List<a, Sigma<&2, a, String, _ => V>>:
  match m:
    case MTip{}:
      acc
    case MLeaf{k, v}:
      (k, v) <> acc
    case MNode{pos, lo, hi}:
      Map.to_list.go(a, V, lo, Map.to_list.go(a, V, hi, acc))

def Map.to_list(a, -V: Kind(a), m: Map<a, V>) ->
  List<a, Sigma<&2, a, String, _ => V>>:
  Map.to_list.go(a, V, m, Nil{})

def Map.keys.go(a, -V: Kind(a), m: Map<a, V>, acc: List<&2, String>) ->
  List<&2, String>:
  match m:
    case MTip{}:
      acc
    case MLeaf{k, v}:
      k <> acc
    case MNode{pos, lo, hi}:
      Map.keys.go(a, V, lo, Map.keys.go(a, V, hi, acc))

def Map.keys(a, -V: Kind(a), m: Map<a, V>) -> List<&2, String>:
  Map.keys.go(a, V, m, Nil{})

def Map.from_list.go(
  a, -V: Kind(a), kvs: List<a, Sigma<&2, a, String, _ => V>>, m: Map<a, V>
) -> Map<a, V>:
  match kvs:
    case Nil{}:
      m
    case (k, v) <> t:
      Map.from_list.go(a, V, t, Map.set(a, V, m, k, v))

def Map.from_list(a, -V: Kind(a), kvs: List<a, Sigma<&2, a, String, _ => V>>) ->
  Map<a, V>:
  Map.from_list.go(a, V, kvs, MTip{})

def Map.union(a, -V: Kind(a), m: Map<a, V>, n: Map<a, V>) -> Map<a, V>:
  Map.from_list.go(a, V, Map.to_list(a, V, n), m)

def Map.size(a, -V: Kind(a), m: Map<a, V>) -> Nat:
  match m:
    case MTip{}:
      0n
    case MLeaf{k, v}:
      1n
    case MNode{pos, lo, hi}:
      Nat.add(Map.size(a, V, lo), Map.size(a, V, hi))

def Map.values.go(a, -V: Kind(a), m: Map<a, V>, acc: List<a, V>) -> List<a, V>:
  match m:
    case MTip{}:
      acc
    case MLeaf{k, v}:
      v <> acc
    case MNode{pos, lo, hi}:
      Map.values.go(a, V, lo, Map.values.go(a, V, hi, acc))

def Map.values(a, -V: Kind(a), m: Map<a, V>) -> List<a, V>:
  Map.values.go(a, V, m, Nil{})

# Set
# ---

def Set() -> Data:
  Map<&2, Unit>

def Set.new() -> Set():
  Map.new(&2, Unit)

def Set.add(s: Set(), key: String) -> Set():
  Map.set(&2, Unit, s, key, Unit{})

def Set.has(s: Set(), key: String) -> Set() & Bool:
  Map.has(&2, Unit, s, key)

def Set.del(s: Set(), key: String) -> Set():
  Map.del(&2, Unit, s, key)

def Set.size(s: Set()) -> Nat:
  Map.size(&2, Unit, s)

def Set.to_list(s: Set()) -> List<&2, String>:
  Map.keys(&2, Unit, s)

def Set.from_list.go(keys: List<&2, String>, s: Set()) -> Set():
  match keys:
    case Nil{}:
      s
    case h <> t:
      Set.from_list.go(t, Set.add(s, h))

def Set.from_list(keys: List<&2, String>) -> Set():
  Set.from_list.go(keys, Set.new())

# Image
# -----

def Image.sink(img: Image) -> Unit:
  Unit{}

def Image.drop.join(a: Unit, b: Unit, c: Unit, d: Unit) -> Unit:
  match a b c d:
    case Unit{} Unit{} Unit{} Unit{}:
      Unit{}

# Forks k levels, then a dead parameter sinks the rest in one loop.
def Image.free(+k: Nat, img: Image) -> Unit:
  match k:
    case 0n:
      Image.sink(img)
    case 1n+e:
      match img:
        case Pix{c}:
          Unit{}
        case Qua{tl, tr, bl, br}:
          a b c d = Image.free(e, tl) Image.free(e, tr) Image.free(e, bl)
            Image.free(e, br)
          Image.drop.join(a, b, c, d)

def Image.drop(img: Image) -> Unit:
  Image.free(7n, img)

# App
# ---

def App.next(
  -S: Type, window: Window, rest: Window -> S -> IO(Unit), next: Maybe<S>
) -> IO(Unit):
  match next:
    case None{}:
      Window.close(window)
    case Some{state}:
      rest(window, state)

def App.turn(
  -S: Type, shown: Window & Image & List<Event>,
  tick: List<Event> -> S -> IO(Maybe<S>), state: S,
  rest: Window -> S -> IO(Unit)
) -> IO(Unit):
  (window, image, events) = shown
  do IO<Unit>:
    Unit <- IO.pure(Unit, Image.drop!(image))
    IO.bind(Maybe<S>, Unit, tick(events, state), App.next(S, window, rest))

def App.draw(
  -S: Type, window: Window, drawn: S & Image,
  tick: List<Event> -> S -> IO(Maybe<S>), rest: Window -> S -> IO(Unit)
) -> IO(Unit):
  (state, image) = drawn
  do IO<Unit>:
    shown : Window & Image & List<Event> <- Window.frame(window, image)
    App.turn(S, shown, tick, state, rest)

def App.step(
  -S: Type, app: App<S>, window: Window, state: S,
  rest: Window -> S -> IO(Unit)
) -> IO(Unit):
  App{view, tick} = app
  App.draw(S, window, view(state), tick, rest)

def App.loop(~S: Type, ~app: App<S>, fuel: Nat, window: Window, state: S) ->
  IO(Unit):
  match fuel:
    case 0n:
      Window.close(window)
    case 1n+f:
      App.step(S, app, window, state, w => s => App.loop(~S, ~app, f, w, s))

def App.run(
  ~S: Type, ~app: App<S>, title: String, width: U32, height: U32, state: S
) -> IO(Unit):
  do IO<Unit>:
    window : Window <- IO.try(Window, Window.open(title, width, height))
    App.loop(~S, ~app, 4294967295n, window, state)

def App.more(-S: Type, rest: S -> IO(Maybe<S>), next: Maybe<S>) ->
  IO(Maybe<S>):
  match next:
    case None{}:
      IO.pure(Maybe<S>, None{})
    case Some{state}:
      rest(state)

def App.fold(
  -S: Type, app: App<S>, events: List<Event>, state: S,
  rest: S -> IO(Maybe<S>)
) -> IO(Maybe<S>):
  App{view, tick} = app
  IO.bind(Maybe<S>, Maybe<S>, tick(events, state), App.more(S, rest))

def App.play(~S: Type, ~app: App<S>, frames: List<List<Event>>, state: S) ->
  IO(Maybe<S>):
  match frames:
    case Nil{}:
      IO.pure(Maybe<S>, Some{state})
    case events <> rest:
      App.fold(S, app, events, state, s => App.play(~S, ~app, rest, s))
