# a copying λ (a λ+) converts with the plain λ of the η-long abstract f
# it instantiates, under a call stuck on m; a λ that uses its variable
# once is a plain λ already
import Base

def apply(m: Maybe<&2, Nat>, f: Nat -> Nat) -> Nat:
  match m:
    case None{}: 0n
    case Some{n}: f(n)

def twice(m: Maybe<&2, Nat>) -> Nat:
  apply(m, +a => Nat.add(a, a))

def once(m: Maybe<&2, Nat>) -> Nat:
  apply(m, a => Nat.add(a, 1n))

def same(m: Maybe<&2, Nat>, -f: Nat -> Nat, -g: Maybe<&2, Nat> -> Nat, e: {apply(m, f) == g(m) : Nat}) -> Unit:
  Unit{}

def t2(+m: Maybe<&2, Nat>) -> Unit:
  same(m, +a => Nat.add(a, a), twice, {==})

def t1(+m: Maybe<&2, Nat>) -> Unit:
  same(m, a => Nat.add(a, 1n), once, {==})

#|ALL PROOFS CHECK
#|Use --verdict for mathematical validity.
