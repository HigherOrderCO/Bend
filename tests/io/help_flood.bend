# io (io_help_flood): 20000 computations each run a user effect on a
# helper at once: the jobs queue on the loop's list under a mutex and
# a condvar, the helpers take them as they free up, and the wake pipe
# carries every answer back; every answer lands and main sums them
# through a channel. The JS lanes run the effect inline: one thread,
# the same sum.
import Base

def Flood.tick(n: U32) -> IO(U32):
  import "./help_flood.c"
  import "./help_flood.js"

law one:
  for c: Chan(U32)
  IO(Unit)

def one(c):
  do IO<Unit>:
    x : U32 <- Flood.tick(1)
    Result<&1, &1, U32, Unit> <- Chan.send(U32, c, x)
    return Unit{}

law spawn_all:
  for n: Nat
  for +c: Chan(U32)
  IO(Unit)

def spawn_all(n, c):
  match n:
    case 0n:
      IO.pure(Unit, Unit{})
    case 1n+p:
      do IO<Unit>:
        Unit <- IO.spawn(Unit, one(c))
        spawn_all(p, c)

law got:
  for m: Maybe<&1, U32>
  U32

def got(m):
  match m:
    case None{}:
      0
    case Some{value}:
      value

law drain:
  for n: Nat
  for +c: Chan(U32)
  for +acc: U32
  IO(U32)

def drain(n, c, acc):
  match n:
    case 0n:
      IO.pure(U32, acc)
    case 1n+p:
      do IO<U32>:
        m : Maybe<&1, U32> <- Chan.recv(U32, c)
        drain(p, c, (acc + got(m) : U32))

law main.go:
  for c: Chan(U32)
  IO(U32)

def main.go(c):
  +ch = c
  do IO<U32>:
    Unit <- spawn_all(U32.to_nat(20000), ch)
    sum : U32 <- drain(U32.to_nat(20000), ch, 0)
    Unit <- IO.print(U32.show(sum))
    return 0

law main:
  IO(U32)

def main():
  IO.bind(Chan(U32), U32, Chan.new(U32, 0), main.go)

#|20000
