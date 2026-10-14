# Rollback netcode: input-synced multiplayer for Bend games over UDP.
#
# A game is an offline game: a state, a pure step(state, inputs) and a
# text codec for its inputs. The net syncs inputs, not states. The relay
# (the Server section; server.bend runs it) stamps every input with its
# index in the room's stream and the server clock, and echoes it to the
# room; every client folds the same stream through the same step, so
# every client computes the same state. A player's own input applies at
# once as a prediction and is replaced by its echo; an echo that lands
# in the past restarts the fold from the last confirmed state (rollback
# and replay). A client shows two states: local (now, with its
# predictions) and remote (a past tick no late input can change).
#
# Time. The relay gives a room a birth time; the room's first tick is
# anchor = tick(born), where tick(ms) = ms * rate / 1000. A post lands
# on tick(max(ctime, stime - tol)): when pressed, if it reached the
# relay within the tolerance, else the earliest tick it may still claim.
#
# Finalization. The engine keeps one folded state (base, at tick) and
# the posts past it. A Pong says the room held `count` posts at server
# time `now`: holding them all, no unseen post lands before
# frontier = now - tol, so the ticks below tick(frontier) fold into
# base, and the posts and predictions on them are dropped. Missing
# posts are asked again with Join{room, next}; a late joiner's Join
# asks for them all.
#
# Wire. A message is one line of words: a tag (A Join, B Send, C Ping,
# D Echo, E Pong) and its fields. The last field of a Send and an Echo
# runs to the end of the line, so an input's text may hold spaces.
import Base

# Types
# =====

# The best (lowest round trip) Ping seen: the server clock read
# `server` when the client clock read `at`. Unsynced while rtt is the
# sentinel.
type Clock is Data:
  Clock{rtt: Nat, at: Nat, server: Nat}

# An input on the room's stream: its index, the tick it lands on, the
# sender's name (the echo match) and the decoded input (None when its
# text does not parse: such a post orders like any other and never
# reaches step). A prediction is a Post with no index yet.
type Post<-P: Data> is Data:
  Post{index: Nat, tick: Nat, name: String, data: Maybe<&2, P>}

# The replay engine. `base` is the fold of every tick through `tick`;
# `anchor` is the room's first tick (0 before the first Pong); `next`
# counts the posts held in a row from index 0; `posts` are received
# and unfolded, ascending by index; `locals` are the predictions, in
# posting order.
type Net<-P: Data, -S: Data> is Data:
  Net{base: S, tick: Nat, anchor: Nat, frontier: Nat, next: Nat,
    posts: List<&2, Post<P>>, locals: List<&2, Post<P>>}

# Client to relay: Join (enter the room; resend from index `from`),
# Send (an input: the room, the synced time, the sender, the text) and
# Ping. Relay to client: Echo (the stamped post) and Pong (the server
# clock, the room's post count as of it, the room's birth).
type Msg is Data:
  Join{room: String, from: Nat}
  Send{room: String, time: Nat, name: String, data: String}
  Ping{room: String, sent: Nat}
  Echo{index: Nat, stime: Nat, ctime: Nat, name: String, data: String}
  Pong{sent: Nat, now: Nat, count: Nat, born: Nat}

# A game as the net sees it: the step, the input codec, the ticks per
# second and the tolerance in ms.
type Game<-P: Data, -S: Data> is Type:
  Game{step: S -> List<&2, P> -> S, read: String -> Maybe<&2, P>,
    show: P -> String, rate: Nat, tol: Nat}

# A client apart from its socket: the relay's address, the room, the
# player's name and next sequence number, the clock, the client time of
# the last Ping and the engine.
type Link<-P: Data, -S: Data> is Data:
  Link{host: String, port: U32, room: String, me: String, seq: Nat,
    clock: Clock, pinged: Nat, net: Net<P, S>}

type Client<-P: Data, -S: Data> is Type:
  Client{sock: Socket, +link: Link<P, S>}

# The relay's rooms: a client's address; a room's birth, post count,
# echo lines newest first and clients.
type Peer is Data:
  Peer{host: String, port: U32}

type Room is Data:
  Room{born: Nat, count: Nat, posts: List<&2, String>, peers: List<&2, Peer>}

# Time
# ====

# Nat.sub saturates, so b + (a - b) is the larger.
def Nat.quo(qr: Nat & Nat) -> Nat:
  (q, r) = qr
  q

# The tick a server time lands on.
def Time.tick(ms: Nat, rate: Nat) -> Nat:
  Nat.quo(Nat.divmod(Nat.mul(ms, rate), 1000n))

# Clock
# =====

def Clock.new() -> Clock:
  Clock{U32.to_nat(4294967295), 0n, 0n}

# The server clock now.
def Clock.server(+c: Clock, now: Nat) -> Nat:
  Clock{rtt, at, server} = c
  Nat.add(server, Nat.sub(now, at))

def Clock.keep(better: Bool, c: Clock, rtt: Nat, at: Nat, server: Nat) -> Clock:
  match better:
    case True{}:
      Clock{rtt, at, server}
    case False{}:
      c

# A Pong sent at client time `sent`, stamped `snow` by the server, back
# at `now`: the server read snow half a round trip before now.
def Clock.sample(+c: Clock, sent: Nat, snow: Nat, +now: Nat) -> Clock:
  Clock{rtt, at, server} = c
  +rtt2 = Nat.sub(now, sent)
  Clock.keep(Nat.is_lt(rtt2, rtt), c, rtt2, now,
    Nat.add(snow, Nat.quo(Nat.divmod(rtt2, 2n))))

# Post
# ====

def Post.index(-P: Data, +p: Post<P>) -> Nat:
  Post{index, tick, name, data} = p
  index

def Post.tick(-P: Data, +p: Post<P>) -> Nat:
  Post{index, tick, name, data} = p
  tick

def Post.name(-P: Data, +p: Post<P>) -> String:
  Post{index, tick, name, data} = p
  name

def Post.insert.go(
  -P: Data, c: Cmp, p: Post<P>, q: Post<P>, t: List<&2, Post<P>>,
  rest: Unit -> List<&2, Post<P>>
) -> List<&2, Post<P>>:
  match c:
    case LT{}:
      p <> q <> t
    case EQ{}:
      q <> t
    case GT{}:
      q <> rest(Unit{})

# Insert by index; a post already held is dropped.
def Post.insert(-P: Data, +p: Post<P>, +ps: List<&2, Post<P>>) ->
  List<&2, Post<P>>:
  match ps:
    case Nil{}:
      [p]
    case q <> t:
      Post.insert.go(P, Nat.cmp(Post.index(P, p), Post.index(P, q)), p, q, t,
        u => Post.insert(P, p, t))

def Post.next.go(c: Cmp, n: Nat, rest: Nat -> Nat) -> Nat:
  match c:
    case LT{}:
      rest(n)
    case EQ{}:
      rest(Nat.add(n, 1n))
    case GT{}:
      n

# The first index missing from the sorted posts, counting from n.
def Post.next(-P: Data, ps: List<&2, Post<P>>, +n: Nat) -> Nat:
  match ps:
    case Nil{}:
      n
    case p <> t:
      Post.next.go(Nat.cmp(Post.index(P, p), n), n, m => Post.next(P, t, m))

def Post.push(-P: Data, ps: List<&2, Post<P>>, p: Post<P>) -> List<&2, Post<P>>:
  match ps:
    case Nil{}:
      [p]
    case q <> t:
      q <> Post.push(P, t, p)

def Post.put(-P: Data, hit: Bool, d: Maybe<&2, P>, acc: List<&2, P>) ->
  List<&2, P>:
  match hit d:
    case True{} Some{x}:
      x <> acc
    case _ _:
      acc

# The inputs of the posts landing on tick t, in list order, before acc.
def Post.at(-P: Data, ps: List<&2, Post<P>>, +t: Nat, acc: List<&2, P>) ->
  List<&2, P>:
  match ps:
    case Nil{}:
      acc
    case p <> rest:
      Post{index, tick, name, data} = p
      Post.put(P, Cmp.is_eq(Nat.cmp(tick, t)), data, Post.at(P, rest, t, acc))

def Post.keep(-P: Data, hit: Bool, p: Post<P>, rest: List<&2, Post<P>>) ->
  List<&2, Post<P>>:
  match hit:
    case True{}:
      p <> rest
    case False{}:
      rest

# The posts landing on tick t or later.
def Post.from(-P: Data, ps: List<&2, Post<P>>, +t: Nat) -> List<&2, Post<P>>:
  match ps:
    case Nil{}:
      []
    case +p <> rest:
      Post.keep(P, Bool.not(Nat.is_lt(Post.tick(P, p), t)), p,
        Post.from(P, rest, t))

# Drop the prediction named: its echo arrived.
def Post.drop(-P: Data, ps: List<&2, Post<P>>, +name: String) -> List<&2, Post<P>>:
  match ps:
    case Nil{}:
      []
    case +p <> rest:
      Post.keep(P, Bool.not(String.eq(Post.name(P, p), name)), p,
        Post.drop(P, rest, name))

# Game
# ====

# A template substitutes ~game as syntax, so a template never opens it:
# these accessors do, on a parameter.
def Game.rate(-P: Data, -S: Data, game: Game<P, S>) -> Nat:
  Game{step, read, show, rate, tol} = game
  rate

def Game.tol(-P: Data, -S: Data, game: Game<P, S>) -> Nat:
  Game{step, read, show, rate, tol} = game
  tol

def Game.step(-P: Data, -S: Data, game: Game<P, S>, state: S, inputs: List<&2, P>)
  -> S:
  Game{step, read, show, rate, tol} = game
  step(state, inputs)

def Game.read(-P: Data, -S: Data, game: Game<P, S>, text: String) -> Maybe<&2, P>:
  Game{step, read, show, rate, tol} = game
  read(text)

def Game.show(-P: Data, -S: Data, game: Game<P, S>, p: P) -> String:
  Game{step, read, show, rate, tol} = game
  show(p)

# Net
# ===

def Net.new(-P: Data, -S: Data, init: S) -> Net<P, S>:
  Net{init, 0n, 0n, 0n, 0n, [], []}

def Net.next(-P: Data, -S: Data, +net: Net<P, S>) -> Nat:
  Net{base, tick, anchor, frontier, next, posts, locals} = net
  next

def Net.anchor(-P: Data, -S: Data, +net: Net<P, S>) -> Nat:
  Net{base, tick, anchor, frontier, next, posts, locals} = net
  anchor

# Step `count` ticks past `from`, each with the inputs landing on it:
# the posts in index order, then the predictions.
def Net.fold(
  ~P: Data, ~S: Data, ~game: Game<P, S>, count: Nat, from: Nat, state: S,
  +ps: List<&2, Post<P>>, +ls: List<&2, Post<P>>
) -> S:
  match count:
    case 0n:
      state
    case 1n+k:
      +t = Nat.add(from, 1n)
      Net.fold(~P, ~S, ~game, k, t,
        Game.step(P, S, game, state, Post.at(P, ps, t, Post.at(P, ls, t, []))),
        ps, ls)

# The state at tick t: base folded forward (base itself at or below
# base's tick).
def Net.state(
  ~P: Data, ~S: Data, ~game: Game<P, S>, base: S, +tick: Nat,
  ps: List<&2, Post<P>>, ls: List<&2, Post<P>>, t: Nat
) -> S:
  Net.fold(~P, ~S, ~game, Nat.sub(t, tick), tick, base, ps, ls)

# The two views: local at tick t with the predictions, remote `lag`
# ticks back from the posts alone.
def Net.views(~P: Data, ~S: Data, ~game: Game<P, S>, +net: Net<P, S>, +t: Nat, lag: Nat)
  -> S & S:
  Net{base, tick, anchor, frontier, next, posts, locals} = net
  (Net.state(~P, ~S, ~game, base, tick, posts, locals, t),
    Net.state(~P, ~S, ~game, base, tick, posts, [], Nat.sub(t, lag)))

def Net.finalize.go(
  ~P: Data, ~S: Data, ~game: Game<P, S>, go: Bool, base: S, +tick: Nat,
  anchor: Nat, frontier: Nat, next: Nat, +ps: List<&2, Post<P>>,
  ls: List<&2, Post<P>>, +target: Nat
) -> Net<P, S>:
  match go:
    case False{}:
      Net{base, tick, anchor, frontier, next, ps, ls}
    case True{}:
      Net{Net.fold(~P, ~S, ~game, Nat.sub(target, tick), tick, base, ps, []),
        target, anchor, frontier, next,
        Post.from(P, ps, Nat.add(target, 1n)), Post.from(P, ls, Nat.add(target, 1n))}

# Fold every tick below the frontier tick into base (posts only: base
# is the same on every client) and drop what landed on them.
def Net.finalize(~P: Data, ~S: Data, ~game: Game<P, S>, +net: Net<P, S>) ->
  Net<P, S>:
  Net{base, tick, anchor, frontier, next, posts, locals} = net
  +target = Nat.sub(Time.tick(frontier, Game.rate(P, S, game)), 1n)
  Net.finalize.go(~P, ~S, ~game, Nat.is_lt(tick, target), base, tick, anchor,
    frontier, next, posts, locals, target)

def Net.bound(gap: Bool, frontier: Nat, bound: Nat) -> Nat:
  match gap:
    case True{}:
      frontier
    case False{}:
      Nat.max(frontier, bound)

# A Pong: the room's birth sets the anchor (and, once, base's tick);
# holding every post the relay counted, the frontier advances.
def Net.pong(
  ~P: Data, ~S: Data, ~game: Game<P, S>, gap: Bool, +net: Net<P, S>, snow: Nat,
  born: Nat
) -> Net<P, S>:
  Net{base, tick, anchor, frontier, next, posts, locals} = net
  +a = Time.tick(born, Game.rate(P, S, game))
  Net.finalize(~P, ~S, ~game,
    Net{base, Nat.max(tick, Nat.sub(a, 1n)), a,
      Net.bound(gap, frontier, Nat.sub(snow, Game.tol(P, S, game))), next, posts,
      locals})

# An echo: the post replaces the prediction of its name. One already
# held is dropped by the insert; one already folded lands at or below
# base's tick and is never stepped.
def Net.post(-P: Data, -S: Data, +net: Net<P, S>, +p: Post<P>) -> Net<P, S>:
  Net{base, tick, anchor, frontier, next, posts, locals} = net
  +ps = Post.insert(P, p, posts)
  Net{base, tick, anchor, frontier, Post.next(P, ps, next), ps,
    Post.drop(P, locals, Post.name(P, p))}

def Net.local(-P: Data, -S: Data, net: Net<P, S>, p: Post<P>) -> Net<P, S>:
  Net{base, tick, anchor, frontier, next, posts, locals} = net
  Net{base, tick, anchor, frontier, next, posts, Post.push(P, locals, p)}

# Msg
# ===

def String.word.go(
  -R: Type, sp: Bool, c: Char, t: String, k: String -> String -> R,
  rest: (String -> String -> R) -> R
) -> R:
  match sp:
    case True{}:
      k(SNil{}, t)
    case False{}:
      rest(w => r => k(SCon{c, w}, r))

# The first word of s and the rest after its space, passed to k.
def String.word(-R: Type, +s: String, k: String -> String -> R) -> R:
  match s:
    case SNil{}:
      k(SNil{}, SNil{})
    case SCon{Chr{c}, t}:
      String.word.go(R, U32.is_eq(c, 32), Chr{c}, t, k, k2 => String.word(R, t, k2))

def Msg.show(m: Msg) -> String:
  match m:
    case Join{room, from}:
      "A " ++ room ++ " " ++ Nat.show(from)
    case Send{room, time, name, data}:
      "B " ++ room ++ " " ++ Nat.show(time) ++ " " ++ name ++ " " ++ data
    case Ping{room, sent}:
      "C " ++ room ++ " " ++ Nat.show(sent)
    case Echo{index, stime, ctime, name, data}:
      "D " ++ Nat.show(index) ++ " " ++ Nat.show(stime) ++ " " ++ Nat.show(ctime)
        ++ " " ++ name ++ " " ++ data
    case Pong{sent, now, count, born}:
      "E " ++ Nat.show(sent) ++ " " ++ Nat.show(now) ++ " " ++ Nat.show(count)
        ++ " " ++ Nat.show(born)

def Msg.join(room: String, from: String) -> Maybe<&2, Msg>:
  do Maybe<&2, Msg>:
    f : Nat <- Nat.read(from)
    return Join{room, f}

def Msg.send(room: String, time: String, name: String, data: String) ->
  Maybe<&2, Msg>:
  do Maybe<&2, Msg>:
    t : Nat <- Nat.read(time)
    return Send{room, t, name, data}

def Msg.ping(room: String, sent: String) -> Maybe<&2, Msg>:
  do Maybe<&2, Msg>:
    t : Nat <- Nat.read(sent)
    return Ping{room, t}

def Msg.echo(index: String, stime: String, ctime: String, name: String, data: String)
  -> Maybe<&2, Msg>:
  do Maybe<&2, Msg>:
    i : Nat <- Nat.read(index)
    s : Nat <- Nat.read(stime)
    c : Nat <- Nat.read(ctime)
    return Echo{i, s, c, name, data}

def Msg.pong(sent: String, now: String, count: String, born: String) ->
  Maybe<&2, Msg>:
  do Maybe<&2, Msg>:
    s : Nat <- Nat.read(sent)
    n : Nat <- Nat.read(now)
    c : Nat <- Nat.read(count)
    b : Nat <- Nat.read(born)
    return Pong{s, n, c, b}

# The tag's offset from 'A' picks the shape; the last word runs to the
# end of the line.
def Msg.read.tag(k: Nat, s: String) -> Maybe<&2, Msg>:
  match k:
    case 0n:
      String.word(Maybe<&2, Msg>, s, room => from => Msg.join(room, from))
    case 1n:
      String.word(Maybe<&2, Msg>, s, room => r1 =>
        String.word(Maybe<&2, Msg>, r1, time => r2 =>
          String.word(Maybe<&2, Msg>, r2, name => data =>
            Msg.send(room, time, name, data))))
    case 2n:
      String.word(Maybe<&2, Msg>, s, room => sent => Msg.ping(room, sent))
    case 3n:
      String.word(Maybe<&2, Msg>, s, index => r1 =>
        String.word(Maybe<&2, Msg>, r1, stime => r2 =>
          String.word(Maybe<&2, Msg>, r2, ctime => r3 =>
            String.word(Maybe<&2, Msg>, r3, name => data =>
              Msg.echo(index, stime, ctime, name, data)))))
    case 4n:
      String.word(Maybe<&2, Msg>, s, sent => r1 =>
        String.word(Maybe<&2, Msg>, r1, now => r2 =>
          String.word(Maybe<&2, Msg>, r2, count => born =>
            Msg.pong(sent, now, count, born))))
    case _:
      None{}

def Msg.tag(tag: String) -> Nat:
  match tag:
    case SCon{Chr{c}, SNil{}}:
      U32.to_nat(U32.sub(c, 65))
    case _:
      5n

def Msg.read(text: String) -> Maybe<&2, Msg>:
  String.word(Maybe<&2, Msg>, text, tag => s => Msg.read.tag(Msg.tag(tag), s))

# Link
# ====

# The server clock now, in ticks.
def Link.tick(~P: Data, ~S: Data, ~game: Game<P, S>, +l: Link<P, S>, now: Nat) -> Nat:
  Link{host, port, room, me, seq, clock, pinged, net} = l
  Time.tick(Clock.server(clock, now), Game.rate(P, S, game))

# How far back the remote view stays: the tolerance plus half a round
# trip, and one tick more.
def Link.lag(~P: Data, ~S: Data, ~game: Game<P, S>, +l: Link<P, S>) -> Nat:
  Link{host, port, room, me, seq, clock, pinged, net} = l
  Clock{rtt, at, server} = clock
  Nat.add(Time.tick(Nat.add(Game.tol(P, S, game), Nat.quo(Nat.divmod(rtt, 2n))),
    Game.rate(P, S, game)), 1n)

def Link.views(~P: Data, ~S: Data, ~game: Game<P, S>, +l: Link<P, S>, now: Nat) ->
  S & S:
  Link{host, port, room, me, seq, clock, pinged, net} = l
  Net.views(~P, ~S, ~game, net, Link.tick(~P, ~S, ~game, l, now),
    Link.lag(~P, ~S, ~game, l))

# The post count and the confirmed state k ticks after the anchor: a
# test hook. Once every post landing by then is held, it is the same
# on every client.
def Link.at(~P: Data, ~S: Data, ~game: Game<P, S>, +l: Link<P, S>, k: Nat) ->
  Nat & S:
  Link{host, port, room, me, seq, clock, pinged, net} = l
  Net{base, tick, anchor, frontier, next, posts, locals} = net
  (next, Net.state(~P, ~S, ~game, base, tick, posts, [], Nat.add(anchor, k)))

# Client
# ======

def Client.sent(sr: Socket & Result<&1, &1, (U32 & String) & String, Unit>) -> IO(Socket):
  (s, r) = sr
  match r:
    case Done{u}:
      IO.pure(Socket, s)
    case Fail{(error, data)}:
      (code, message) = error
      IO.die(Socket, code, message)

# Send a message to the relay; a failed send is fatal.
def Client.send(sock: Socket, host: String, port: U32, m: Msg) -> IO(Socket):
  do IO<Socket>:
    sr : Socket & Result<&1, &1, (U32 & String) & String, Unit> <-
      UDP.send_to(sock, host, port, Msg.show(m))
    Client.sent(sr)

def Client.mend(gap: Bool, sock: Socket, host: String, port: U32, room: String,
  from: Nat) -> IO(Socket):
  match gap:
    case False{}:
      IO.pure(Socket, sock)
    case True{}:
      Client.send(sock, host, port, Join{room, from})

# A Pong: a clock sample and a checkpoint; missing posts are asked
# again from the first missing index.
def Client.pong(
  ~P: Data, ~S: Data, ~game: Game<P, S>, c: Client<P, S>, sent: Nat, +snow: Nat,
  count: Nat, born: Nat, +now: Nat
) -> IO(Client<P, S>):
  Client{sock, link} = c
  Link{host, port, room, me, seq, clock, pinged, net} = link
  +gap = Nat.is_lt(Net.next(P, S, net), count)
  do IO<Client<P, S>>:
    s2 : Socket <- Client.mend(gap, sock, host, port, room, Net.next(P, S, net))
    return Client{s2, Link{host, port, room, me, seq,
      Clock.sample(clock, sent, snow, now), pinged,
      Net.pong(~P, ~S, ~game, gap, net, snow, born)}}

def Client.echo(
  ~P: Data, ~S: Data, ~game: Game<P, S>, c: Client<P, S>, index: Nat, stime: Nat,
  ctime: Nat, name: String, data: String
) -> Client<P, S>:
  Client{sock, link} = c
  Link{host, port, room, me, seq, clock, pinged, net} = link
  Client{sock, Link{host, port, room, me, seq, clock, pinged,
    Net.post(P, S, net, Post{index,
      Time.tick(Nat.max(ctime, Nat.sub(stime, Game.tol(P, S, game))),
        Game.rate(P, S, game)),
      name, Game.read(P, S, game, data)})}}

# One datagram from the relay.
def Client.take(
  ~P: Data, ~S: Data, ~game: Game<P, S>, m: Maybe<&2, Msg>, c: Client<P, S>,
  now: Nat
) -> IO(Client<P, S>):
  match m:
    case Some{Echo{index, stime, ctime, name, data}}:
      IO.pure(Client<P, S>,
        Client.echo(~P, ~S, ~game, c, index, stime, ctime, name, data))
    case Some{Pong{sent, snow, count, born}}:
      Client.pong(~P, ~S, ~game, c, sent, snow, count, born, now)
    case _:
      IO.pure(Client<P, S>, c)

def Client.got(
  ~P: Data, ~S: Data, ~game: Game<P, S>, got: Maybe<&1, String & U32 & String>,
  c: Client<P, S>, now: Nat, rest: Client<P, S> -> IO(Client<P, S>)
) -> IO(Client<P, S>):
  match got:
    case None{}:
      IO.pure(Client<P, S>, c)
    case Some{(from, port, text)}:
      IO.bind(Client<P, S>, Client<P, S>,
        Client.take(~P, ~S, ~game, Msg.read(text), c, now), rest)

def Client.polled(
  ~P: Data, ~S: Data, ~game: Game<P, S>,
  sr: Socket & Poll<&1, &1, Result<&1, &1, U32 & String, String & U32 & String>, Unit>,
  link: Link<P, S>, now: Nat, rest: Client<P, S> -> IO(Client<P, S>)
) -> IO(Client<P, S>):
  (s2, r) = sr
  match r:
    case Ready{res}:
      do IO<Client<P, S>>:
        got : String & U32 & String <- IO.pass(String & U32 & String, res)
        Client.got(~P, ~S, ~game, Some{got}, Client{s2, link}, now, rest)
    case Wait{u}:
      Client.got(~P, ~S, ~game, None{}, Client{s2, link}, now, rest)

# Take every datagram waiting on the socket (at most `fuel`).
def Client.drain(
  ~P: Data, ~S: Data, ~game: Game<P, S>, fuel: Nat, c: Client<P, S>, +now: Nat
) -> IO(Client<P, S>):
  match fuel:
    case 0n:
      IO.pure(Client<P, S>, c)
    case 1n+f:
      Client{sock, link} = c
      do IO<Client<P, S>>:
        sr : Socket & Poll<&1, &1, Result<&1, &1, U32 & String, String & U32 & String>, Unit>
          <- UDP.try_recv_from(sock, 2048, 0)
        Client.polled(~P, ~S, ~game, sr, link, now,
          c2 => Client.drain(~P, ~S, ~game, f, c2, now))

# A Ping every half second.
def Client.ping(-P: Data, -S: Data, due: Bool, sock: Socket, +l: Link<P, S>, +now: Nat)
  -> IO(Client<P, S>):
  match due:
    case False{}:
      IO.pure(Client<P, S>, Client{sock, l})
    case True{}:
      Link{host, port, room, me, seq, clock, pinged, net} = l
      do IO<Client<P, S>>:
        s2 : Socket <- Client.send(sock, host, port, Ping{room, now})
        return Client{s2, Link{host, port, room, me, seq, clock, now, net}}

def Client.pinged(-P: Data, -S: Data, c: Client<P, S>, +now: Nat) -> IO(Client<P, S>):
  Client{sock, link} = c
  Link{host, port, room, me, seq, clock, pinged, net} = link
  Client.ping(P, S, Nat.is_lt(Nat.add(pinged, 500n), now), sock, link, now)

# Take what the relay sent and ping when due.
def Client.pump(~P: Data, ~S: Data, ~game: Game<P, S>, c: Client<P, S>, +now: Nat)
  -> IO(Client<P, S>):
  do IO<Client<P, S>>:
    c2 : Client<P, S> <- Client.drain(~P, ~S, ~game, 256n, c, now)
    Client.pinged(P, S, c2, now)

def Client.viewed(~P: Data, ~S: Data, ~game: Game<P, S>, c: Client<P, S>, now: Nat)
  -> Client<P, S> & S & S:
  Client{sock, link} = c
  (Client{sock, link}, Link.views(~P, ~S, ~game, link, now))

def Client.frame.go(~P: Data, ~S: Data, ~game: Game<P, S>, c: Client<P, S>, +now: Nat)
  -> IO(Client<P, S> & S & S):
  do IO<Client<P, S> & S & S>:
    c2 : Client<P, S> <- Client.pump(~P, ~S, ~game, c, now)
    return Client.viewed(~P, ~S, ~game, c2, now)

# Once per frame: take what the relay sent, ping when due, and answer
# the client with its local and remote states.
def Client.frame(~P: Data, ~S: Data, ~game: Game<P, S>, c: Client<P, S>) ->
  IO(Client<P, S> & S & S):
  do IO<Client<P, S> & S & S>:
    now : Nat <- IO.now()
    Client.frame.go(~P, ~S, ~game, c, now)

def Client.post.go(
  ~P: Data, ~S: Data, ~game: Game<P, S>, c: Client<P, S>, +p: P, now: Nat
) -> IO(Client<P, S>):
  Client{sock, link} = c
  Link{host, port, room, me, seq, clock, pinged, net} = link
  +name = me ++ "-" ++ Nat.show(seq)
  +time = Clock.server(clock, now)
  do IO<Client<P, S>>:
    s2 : Socket <- Client.send(sock, host, port,
      Send{room, time, name, Game.show(P, S, game, p)})
    return Client{s2, Link{host, port, room, me, Nat.add(seq, 1n), clock, pinged,
      Net.local(P, S, net,
        Post{0n, Time.tick(time, Game.rate(P, S, game)), name, Some{p}})}}

# Post an input: predicted at once, sent to the relay.
def Client.post(~P: Data, ~S: Data, ~game: Game<P, S>, c: Client<P, S>, p: P) ->
  IO(Client<P, S>):
  do IO<Client<P, S>>:
    now : Nat <- IO.now()
    Client.post.go(~P, ~S, ~game, c, p, now)

def Client.sync.go(
  ~P: Data, ~S: Data, ~game: Game<P, S>, ok: Bool, c: Client<P, S>,
  rest: Client<P, S> -> IO(Client<P, S>)
) -> IO(Client<P, S>):
  match ok:
    case True{}:
      IO.pure(Client<P, S>, c)
    case False{}:
      do IO<Client<P, S>>:
        Unit <- IO.sleep(5)
        rest(c)

def Client.sync.fin(
  ~P: Data, ~S: Data, ~game: Game<P, S>, c: Client<P, S>,
  rest: Client<P, S> -> IO(Client<P, S>)
) -> IO(Client<P, S>):
  Client{sock, link} = c
  Link{host, port, room, me, seq, clock, pinged, net} = link
  Client.sync.go(~P, ~S, ~game, Nat.is_lt(0n, Net.anchor(P, S, net)),
    Client{sock, link}, rest)

def Client.sync.step(
  ~P: Data, ~S: Data, ~game: Game<P, S>, c: Client<P, S>, +now: Nat,
  rest: Client<P, S> -> IO(Client<P, S>)
) -> IO(Client<P, S>):
  do IO<Client<P, S>>:
    c2 : Client<P, S> <- Client.pump(~P, ~S, ~game, c, now)
    Client.sync.fin(~P, ~S, ~game, c2, rest)

# Pump (the first pump pings) until the relay's first Pong sets the
# clock and the anchor, which every post needs: two seconds at most.
def Client.sync(
  ~P: Data, ~S: Data, ~game: Game<P, S>, fuel: Nat, c: Client<P, S>
) -> IO(Client<P, S>):
  match fuel:
    case 0n:
      IO.die(Client<P, S>, 1, "netcode: the relay did not answer (is it running?)")
    case 1n+f:
      do IO<Client<P, S>>:
        now : Nat <- IO.now()
        Client.sync.step(~P, ~S, ~game, c, now, c2 => Client.sync(~P, ~S, ~game, f, c2))

def Client.hello(
  -P: Data, -S: Data, init: S, sock: Socket, +host: String, +port: U32,
  +room: String, me: String
) -> IO(Client<P, S>):
  do IO<Client<P, S>>:
    s2 : Socket <- Client.send(sock, host, port, Join{room, 0n})
    return Client{s2, Link{host, port, room, me, 0n, Clock.new(), 0n,
      Net.new(P, S, init)}}

# Bind a port, join the room and sync. `me` names the player: its posts
# are "me-seq".
def Client.open(
  ~P: Data, ~S: Data, ~game: Game<P, S>, init: S, host: String, port: U32,
  room: String, me: String
) -> IO(Client<P, S>):
  do IO<Client<P, S>>:
    sock : Socket <- IO.try(Socket, UDP.bind("0.0.0.0", 0))
    c : Client<P, S> <- Client.hello(P, S, init, sock, host, port, room, me)
    Client.sync(~P, ~S, ~game, 400n, c)

# The post count and the confirmed state k ticks after the anchor.
def Client.at(~P: Data, ~S: Data, ~game: Game<P, S>, c: Client<P, S>, k: Nat) ->
  Client<P, S> & Nat & S:
  Client{sock, link} = c
  (Client{sock, link}, Link.at(~P, ~S, ~game, link, k))

def Client.close(-P: Data, -S: Data, c: Client<P, S>) -> IO(Unit):
  Client{sock, link} = c
  Socket.close(sock)

# Env
# ===

def Env.got(r: Result<&1, &1, U32 & String, String>, default: String) -> String:
  match r:
    case Done{text}:
      text
    case Fail{e}:
      default

# An environment variable, or the default.
def Env.get(name: String, default: String) -> IO(String):
  do IO<String>:
    r : Result<&1, &1, U32 & String, String> <- IO.get_env(name)
    return Env.got(r, default)

def Env.u32.go(m: Maybe<&2, U32>, default: U32) -> U32:
  match m:
    case None{}:
      default
    case Some{v}:
      v

def Env.u32(text: String, default: U32) -> U32:
  Env.u32.go(U32.read(text), default)

# Server
# ======

# The relay. A Join enters a room and asks for its posts from index
# `from` (0 for all of them: a late joiner catches up, a lost echo is
# mended). A Send is stamped with its index and the server clock and
# echoed to every client in the room, the sender included (the echo
# confirms its prediction). A Ping is answered with a Pong: the clock
# sync and the checkpoint in one. The relay reads no input's text and
# runs no game logic; one computation serves every room, a datagram in
# and a few out.

def Peer.eq(+a: Peer, +b: Peer) -> Bool:
  Peer{h1, p1} = a
  Peer{h2, p2} = b
  Bool.and(String.eq(h1, h2), U32.is_eq(p1, p2))

def Peer.has(ps: List<&2, Peer>, +p: Peer) -> Bool:
  match ps:
    case Nil{}:
      False{}
    case q <> t:
      Bool.or(Peer.eq(q, p), Peer.has(t, p))

def Peer.add(known: Bool, ps: List<&2, Peer>, p: Peer) -> List<&2, Peer>:
  match known:
    case True{}:
      ps
    case False{}:
      p <> ps

def Server.sent(sr: Socket & Result<&1, &1, (U32 & String) & String, Unit>) -> IO(Socket):
  (s, r) = sr
  IO.pure(Socket, s)

# A failed send to one client is that client's problem, not the room's.
def Server.send(sock: Socket, +p: Peer, line: String) -> IO(Socket):
  Peer{host, port} = p
  do IO<Socket>:
    sr : Socket & Result<&1, &1, (U32 & String) & String, Unit> <-
      UDP.send_to(sock, host, port, line)
    Server.sent(sr)

def Server.cast(peers: List<&2, Peer>, sock: Socket, +line: String) -> IO(Socket):
  match peers:
    case Nil{}:
      IO.pure(Socket, sock)
    case p <> t:
      do IO<Socket>:
        s2 : Socket <- Server.send(sock, p, line)
        Server.cast(t, s2, line)

# The newest n lines to one client.
def Server.resend(lines: List<&2, String>, n: Nat, sock: Socket, +p: Peer) ->
  IO(Socket):
  match lines n:
    case Nil{} _:
      IO.pure(Socket, sock)
    case l <> t 0n:
      IO.pure(Socket, sock)
    case l <> t 1n+k:
      do IO<Socket>:
        s2 : Socket <- Server.send(sock, p, l)
        Server.resend(t, k, s2, p)

def Server.join.go(
  sock: Socket, +peer: Peer, room: String, from: Nat, rooms: Map<&2, Room>, +r: Room
) -> IO(Socket & Map<&2, Room>):
  Room{born, count, posts, peers} = r
  do IO<Socket & Map<&2, Room>>:
    s2 : Socket <- Server.resend(posts, Nat.sub(count, from), sock, peer)
    return (s2, Map.set(&2, Room, rooms, room,
      Room{born, count, posts, Peer.add(Peer.has(peers, peer), peers, peer)}))

def Server.join(
  sock: Socket, peer: Peer, room: String, from: Nat, rr: Map<&2, Room> & Room
) -> IO(Socket & Map<&2, Room>):
  (rooms, r) = rr
  Server.join.go(sock, peer, room, from, rooms, r)

# The post's index is the room's count; its client time is clamped to
# the room's birth, so every post lands at the anchor or after.
def Server.post.go(
  sock: Socket, room: String, ctime: Nat, name: String, data: String, now: Nat,
  rooms: Map<&2, Room>, +r: Room
) -> IO(Socket & Map<&2, Room>):
  Room{born, count, posts, peers} = r
  +line = Msg.show(Echo{count, now, Nat.max(ctime, born), name, data})
  do IO<Socket & Map<&2, Room>>:
    s2 : Socket <- Server.cast(peers, sock, line)
    return (s2, Map.set(&2, Room, rooms, room,
      Room{born, Nat.add(count, 1n), line <> posts, peers}))

def Server.post(
  sock: Socket, room: String, ctime: Nat, name: String, data: String, now: Nat,
  rr: Map<&2, Room> & Room
) -> IO(Socket & Map<&2, Room>):
  (rooms, r) = rr
  Server.post.go(sock, room, ctime, name, data, now, rooms, r)

def Server.ping(
  sock: Socket, +peer: Peer, sent: Nat, now: Nat, rr: Map<&2, Room> & Room
) -> IO(Socket & Map<&2, Room>):
  (rooms, r) = rr
  Room{born, count, posts, peers} = r
  do IO<Socket & Map<&2, Room>>:
    s2 : Socket <- Server.send(sock, peer, Msg.show(Pong{sent, now, count, born}))
    return (s2, rooms)

def Server.serve(
  m: Maybe<&2, Msg>, sock: Socket, rooms: Map<&2, Room>, +peer: Peer, +now: Nat
) -> IO(Socket & Map<&2, Room>):
  match m:
    case Some{Join{+room, from}}:
      Server.join(sock, peer, room, from,
        Map.get(Room, Room{now, 0n, [], []}, rooms, room))
    case Some{Send{+room, time, name, data}}:
      Server.post(sock, room, time, name, data, now,
        Map.get(Room, Room{now, 0n, [], []}, rooms, room))
    case Some{Ping{room, sent}}:
      Server.ping(sock, peer, sent, now,
        Map.get(Room, Room{now, 0n, [], []}, rooms, room))
    case _:
      IO.pure(Socket & Map<&2, Room>, (sock, rooms))

def Server.step(got: String & U32 & String, sock: Socket, rooms: Map<&2, Room>) ->
  IO(Socket & Map<&2, Room>):
  (host, port, text) = got
  do IO<Socket & Map<&2, Room>>:
    now : Nat <- IO.now()
    Server.serve(Msg.read(text), sock, rooms, Peer{host, port}, now)

def Server.recv(
  sr: Socket & Result<&1, &1, U32 & String, String & U32 & String>,
  rooms: Map<&2, Room>
) -> IO(Socket & Map<&2, Room>):
  (sock, r) = sr
  do IO<Socket & Map<&2, Room>>:
    got : String & U32 & String <- IO.pass(String & U32 & String, r)
    Server.step(got, sock, rooms)

# Serve until killed.
@unsafe
def Server.loop(sm: Socket & Map<&2, Room>) -> IO(Unit):
  (sock, rooms) = sm
  do IO<Unit>:
    sr : Socket & Result<&1, &1, U32 & String, String & U32 & String> <-
      UDP.recv_from(sock, 2048)
    sm2 : Socket & Map<&2, Room> <- Server.recv(sr, rooms)
    Server.loop(sm2)

def Server.run(+port: U32) -> IO(Unit):
  do IO<Unit>:
    Unit <- IO.print("relay on 0.0.0.0:" ++ U32.show(port))
    sock : Socket <- IO.try(Socket, UDP.bind("0.0.0.0", port))
    Server.loop((sock, Map.new(&2, Room)))
