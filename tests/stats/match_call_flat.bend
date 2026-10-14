# bend3's match-on-call fence, respelled for the banned form: the
# 12-digit scanner threads its accumulator and stop-state as a tuple
# parameter (a match on a call is a banned form now), each guard a
# worker def -- the scanned digit and accumulator ride +U32
# binders, the descent on the string column
import Base

type D6 is Data:
  D6{n: U32, src: String}

law dig_fits:
  for acc: U32
  Bool

def dig_fits(acc):
  U32.is_lt(acc, 429496729)

law int_go.rev:
  for s   : String
  for acc : String
  String

def int_go.rev(s, acc):
  match s:
    case SNil{}:
      acc
    case SCon{c, t}:
      int_go.rev(t, SCon{c, acc})

law int_go.fits:
  for +n   : U32
  for +acc : U32
  for kept : String
  for z    : Bool
  U32 & Bool & String

def int_go.fits(n, acc, kept, z):
  match z:
    case False{}:
      (acc, True{}, kept)
    case True{}:
      ((acc * 10 + (n - 48) : U32), True{}, kept)

law int_go.digit:
  for +n   : U32
  for +acc : U32
  for kept : String
  for d    : Bool
  U32 & Bool & String

def int_go.digit(n, acc, kept, d):
  match d:
    case False{}:
      (acc, False{}, SCon{Chr{n}, kept})
    case True{}:
      int_go.fits(n, acc, kept, dig_fits(acc))

law int_go.step:
  for +n   : U32
  for +acc : U32
  for kept : String
  for live : Bool
  for d    : Bool
  U32 & Bool & String

def int_go.step(n, acc, kept, live, d):
  match live:
    case False{}:
      (acc, False{}, SCon{Chr{n}, kept})
    case True{}:
      int_go.digit(n, acc, kept, d)

law int_go:
  for src : String
  for st  : U32 & Bool & String
  D6

def int_go(src, st):
  match src:
    case SCon{Chr{n0}, t}:
      (acc0, live, kept) = st
      +cn = {n0 : U32}
      +acc = {acc0 : U32}
      int_go(t, int_go.step(cn, acc, kept, live,
        U32.is_ge(cn, 48) && U32.is_le(cn, 57)))
    case SNil{}:
      (acc, live, kept) = st
      D6{acc, int_go.rev(kept, SNil{})}

# twelve 9s: the accumulator stops at 999999999 (the next digit would
# overflow U32) and the dropped digits leave nothing kept
law t6:
  String

law t6.fin:
  for w: D6
  String

def t6.fin(w):
  match w:
    case D6{v, s2}:
      String.append(U32.show(v), String.append(" [", String.append(s2, "]")))

def t6():
  t6.fin(int_go("999999999999", (0, True{}, SNil{})))

law main:
  IO(Unit)

def main():
  IO.print(t6())

#|999999999 []
