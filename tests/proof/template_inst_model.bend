# use's ~F is an injectivity proof, which no built model proves; main's
# instance passes inj there, so inj is its model and use is still checked
# at an opaque F. use2's ~P must fit ~f: both come from the one instance,
# where a model built for f (n => Z{}) would leave none for P. app's
# instance sets app~f to pred, so use4 cannot share it: use4's R holds
# only at its own g (n => Z{})
import Base

type N is Data:
  Z{}
  S{p: N}

def pred(n: N) -> N:
  match n:
    case Z{}: Z{}
    case S{p}: p

def inj(a: N, b: N, e: {S{a} == S{b} : N}) -> {a == b : N}:
  %x@e : {pred(S{a}) == pred(_) : N}
  {==}

def use(~F: @a: N -> @b: N -> {S{a} == S{b} : N} -> {a == b : N},
  a: N, b: N, e: {S{a} == S{b} : N}) -> {a == b : N}:
  F(a, b, e)

def inj2(a: N, b: N, e: {pred(S{a}) == pred(S{b}) : N}) -> {a == b : N}:
  e

def use2(~f: N -> N, ~P: @a: N -> @b: N -> {f(S{a}) == f(S{b}) : N} -> {a == b : N},
  a: N, b: N, e: {f(S{a}) == f(S{b}) : N}) -> {a == b : N}:
  P(a, b, e)

def app(~f: N -> N, x: N) -> N:
  f(x)

def use4(~g: N -> N, ~R: @a: N -> {g(a) == Z{} : N}, a: N) -> {g(a) == Z{} : N}:
  R(a)

def main() -> {Z{} == Z{} : N} & {Z{} == Z{} : N} & N:
  (use(~inj, Z{}, Z{}, {==}), use2(~pred, ~inj2, Z{}, Z{}, {==}), app(~pred, S{Z{}}))

#|({==}, {==}, Z{})
