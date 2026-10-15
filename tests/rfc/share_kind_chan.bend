# rfc (rfc_share_kind_chan): share_kind with the type function named
# Chan. A shared Chan(A) heats nothing only when Chan is the handle law;
# here Chan is the def's own erased parameter, a stuck application like
# F(A), so the keep must still heat every site type, or the Maybe<Duo>
# built from a runtime zero stays cold and opens raw behind the count-2
# cell (a memory fault in C). Expect 6n on every lane.
import Base

type Duo is Data:
  Dd{a: Nat, b: Nat}

law opn:
  for m: Maybe<&2, Duo>
  Nat

def opn(m):
  match m:
    case Some{Dd{a, b}}:
      Nat.add(a, b)
    case None{}:
      0n

law twice:
  for -Chan: Data -> Data
  for -A: Data
  for +x: Chan(A)
  for f: Chan(A) -> Nat
  for g: Chan(A) -> Nat
  Nat

def twice(Chan, A, x, f, g):
  Nat.add(f(x), g(x))

law go:
  for +z: Nat
  Nat

def go(z):
  twice(A => Maybe<&2, A>, Duo, Some{Dd{Nat.add(z, 1n), Nat.add(z, 2n)}}, opn, opn)

def main() -> IO(Unit):
  do IO<Unit>:
    +t : Nat <- IO.now()
    IO.print(String.append(Nat.show(go(Nat.sub(t, t))), "n"))

#|6n
