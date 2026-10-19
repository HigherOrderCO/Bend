# contraction is growth: bend3's NSI ledger refused loop because
# append(p, p) doubles what the match released. bend counts the same
# fact at the binder itself: p is affine, so its second spend in
# append(p, p) is refused in grow (loop itself descends plainly).
# ported from bend3: bare ctors, main returns the test's own type
type T is Data:
  S{p: T}
  Z{}

law append:
  for a: T
  for b: T
  T

def append(a, b):
  match b:
    case S{q}:
      S{append(a, q)}
    case b:
      a

law grow:
  for t: T
  T

def grow(t):
  match t:
    case S{p}:
      S{append(p, p)}
    case t:
      t

law loop:
  for t: T
  T

def loop(t):
  match t:
    case S{x}:
      loop(x)
    case w:
      w

law main:
  T

def main(): Z{}

#|SOME PROOFS FAIL
#|Error:
#|- message  : p is used 2 times (at 29:16, 29:19), but a plain binding is used at most once
#|Location: grow
#|28 |     case S{p}:
#|29>|       S{append(p, p)}
#|   |                   ^
#|30 |     case t:
#|Note: a +p binding may be used many times if its type is Data; otherwise use it once (a function or another Type value cannot be copied).
#|Note: p is bound by a pattern: put the + on the matched parameter, +t.
#|exit 1
