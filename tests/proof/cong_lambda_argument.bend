# --verdict: congruence over a function argument, bound by a + let
# (#1133). The function goes to the kernel as an η-long λ, and the
# let's type, checked as Data, applies it: to a call, to a constructor,
# to one argument of two (a partial application), or to two. The kernel
# puts a λ argument into a type annotated with its domain, so those
# applications infer.
import Base

law cong2:
  for -A: Type
  for -B: Type
  for -f: A -> A -> B
  for -a: A
  for -b: A
  for -y: A
  for  e: {a == b : A}
  {f(a, y) == f(b, y) : B}

def cong2(A, B, f, a, b, y, e):
  %e : {f(a, y) == f(_, y) : B}
  {==}

def snd_of(p: U32 & Bool) -> Bool:
  (a, b) = p
  b

def fst_snd(p: U32 & Bool, q: U32 & Bool) -> Bool:
  (a, b) = p
  b

def mk(n: U32) -> U32 & Bool:
  (n, True{})

def unary(+n: U32, x: U32 & Bool, +h: {x == mk(n) : U32 & Bool}) -> {snd_of(x) == snd_of(mk(n)) : Bool}:
  +h2 = Equal.cong(U32 & Bool, Bool, snd_of, x, mk(n), h)
  h2

def binary(+n: U32, x: U32 & Bool, +h: {x == mk(n) : U32 & Bool}) -> {fst_snd(x, mk(0)) == fst_snd(mk(n), mk(0)) : Bool}:
  +h2 = cong2(U32 & Bool, Bool, fst_snd, x, mk(n), mk(0), h)
  h2

def partial(+n: U32, x: U32 & Bool, +h: {x == mk(n) : U32 & Bool}) -> {fst_snd(x) == fst_snd(mk(n)) : U32 & Bool -> Bool}:
  +h2 = Equal.cong(U32 & Bool, U32 & Bool -> Bool, fst_snd, x, mk(n), h)
  h2

def ctor(+x: Bool, +h: {True{} == x : Bool}) -> {Bool.not(True{}) == Bool.not(x) : Bool}:
  +h2 = Equal.cong(Bool, Bool, Bool.not, True{}, x, h)
  h2

def main() -> IO(Unit):
  IO.print(Bool.show(snd_of(mk(1))))

#|True
