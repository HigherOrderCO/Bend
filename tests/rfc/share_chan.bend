# rfc (rfc_share_chan): a shared Chan heats nothing. Chan(A) applies a
# law, a stuck application, so a keep of one used to fall back to heating
# every site type (the * fact): one +c made every datatype hot, and a List
# that nothing shares sealed every cell it built. A copy of a channel
# copies its handle, which Chan.new mints counted, and the effects move
# each value in and out, so nothing needs heating. A value still arrives
# behind the count-1 cell a hot Some, Ready or Fail puts in front of it:
# twice keeps a Rope built from what a second channel delivers (no fold
# removes the keep, and the Duos are built from a zero it delivers, so
# none is a static node), which heats Maybe, Poll and Result through
# Knot's fields; the site-type fixpoint then heats Duo through the binders of
# what Chan.recv, Chan.try_recv and Chan.send answer, and every Duo that
# crosses the channel opens through ctr_take: wrap owns it, so a raw open
# would read the count-1 cell as the node. Ask keeps a record that holds
# the channel. Expect 499500, 30, 18, 3, 7, 11, 0 and 15 on every
# lane.
import Base

type Duo is Data:
  Dz{}
  Dd{a: U32, b: U32, rest: Duo}

type Ask is Data:
  Ask{n: U32, reply: Chan(Duo)}

def build(n: Nat, +i: U32, acc: List<U32>) -> List<U32>:
  match n:
    case 0n:
      acc
    case 1n+p:
      build(p, U32.add(i, 1), i <> acc)

def sum(xs: List<U32>, acc: U32) -> U32:
  match xs:
    case Nil{}:
      acc
    case x <> t:
      sum(t, U32.add(acc, x))

type Rope is Data:
  End{}
  Knot{m: Maybe<&2, U32>, p: Poll<&2, &2, U32, U32>, r: Result<&2, &2, U32, U32>, rest: Rope}

def rope(v: Maybe<&1, U32>) -> Rope:
  match v:
    case Some{+x}:
      Knot{Some{x}, Ready{U32.add(x, 1)}, Done{U32.add(x, 2)}, End{}}
    case None{}:
      End{}

def mval(m: Maybe<&2, U32>) -> U32:
  match m:
    case Some{v}:
      v
    case None{}:
      0

def pval(p: Poll<&2, &2, U32, U32>) -> U32:
  match p:
    case Ready{v}:
      v
    case Wait{r}:
      r

def rval(r: Result<&2, &2, U32, U32>) -> U32:
  match r:
    case Done{v}:
      v
    case Fail{e}:
      e

def num(m: Maybe<&1, U32>) -> U32:
  match m:
    case Some{v}:
      v
    case None{}:
      0

def rsum(r: Rope) -> U32:
  match r:
    case End{}:
      0
    case Knot{m, p, q, rest}:
      U32.add(U32.add(mval(m), pval(p)), U32.add(rval(q), rsum(rest)))

def twice(+r: Rope) -> U32:
  U32.add(rsum(Knot{None{}, Wait{0}, Fail{0}, r}), rsum(r))

def ask_n(q: Ask) -> U32:
  match q:
    case Ask{n, reply}:
      n

def wrap(d: Duo) -> Duo:
  match d:
    case Dz{}:
      Dz{}
    case Dd{a, b, rest}:
      Dd{U32.add(a, b), 0, rest}

def dsum(d: Duo) -> U32:
  match d:
    case Dz{}:
      0
    case Dd{a, b, rest}:
      U32.add(U32.add(a, b), dsum(rest))

def got(m: Maybe<&1, Duo>) -> U32:
  match m:
    case Some{d}:
      dsum(wrap(d))
    case None{}:
      0

def got_poll(p: Poll<&1, &1, Maybe<&1, Duo>, Unit>) -> U32:
  match p:
    case Ready{m}:
      got(m)
    case Wait{u}:
      99

def got_fail(r: Result<&1, &1, Duo, Unit>) -> U32:
  match r:
    case Fail{d}:
      dsum(wrap(d))
    case Done{u}:
      99

def say(n: U32) -> IO(Unit):
  IO.print(U32.show(n))

def main() -> IO(Unit):
  do IO<Unit>:
    Unit <- say(sum(build(1000n, 0, Nil{}), 0))
    +k : Chan(U32) <- Chan.new(U32, 2)
    t4 : Result<&1, &1, U32, Unit> <- Chan.send(U32, k, 4)
    t0 : Result<&1, &1, U32, Unit> <- Chan.send(U32, k, 0)
    v : Maybe<&1, U32> <- Chan.recv(U32, k)
    Unit <- say(twice(rope(v)))
    w : Maybe<&1, U32> <- Chan.recv(U32, k)
    +z : U32 <- IO.pure(U32, num(w))
    +c : Chan(Duo) <- Chan.new(Duo, 3)
    +q : Ask <- IO.pure(Ask, Ask{9, c})
    Unit <- say(U32.add(ask_n(q), ask_n(q)))
    s1 : Result<&1, &1, Duo, Unit> <- Chan.send(Duo, c, Dd{U32.add(z, 1), 2, Dz{}})
    s2 : Result<&1, &1, Duo, Unit> <- Chan.send(Duo, c, Dd{U32.add(z, 3), 4, Dz{}})
    s3 : Result<&1, &1, Duo, Unit> <- Chan.send(Duo, c, Dd{U32.add(z, 5), 6, Dz{}})
    m1 : Maybe<&1, Duo> <- Chan.recv(Duo, c)
    Unit <- say(got(m1))
    p2 : Poll<&1, &1, Maybe<&1, Duo>, Unit> <- Chan.try_recv(Duo, c, 0)
    Unit <- say(got_poll(p2))
    Unit <- Chan.close(Duo, c)
    m3 : Maybe<&1, Duo> <- Chan.recv(Duo, c)
    Unit <- say(got(m3))
    m4 : Maybe<&1, Duo> <- Chan.recv(Duo, c)
    Unit <- say(got(m4))
    f : Result<&1, &1, Duo, Unit> <- Chan.send(Duo, c, Dd{U32.add(z, 7), 8, Dz{}})
    say(got_fail(f))

#|499500
#|30
#|18
#|3
#|7
#|11
#|0
#|15
