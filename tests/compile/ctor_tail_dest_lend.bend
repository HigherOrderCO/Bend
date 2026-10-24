# A call built after its constructor leaves no frame, so it can't lend: copy
# borrows its list (lend still reads xs after it), yet wrap, pair and wrap2
# end in a copy past the list's last use. wrap2's other field owns the list
# too, so it builds after the call. Build with
# CCC_OVERRIDE_OPTIONS=+-DBEND_POISON to fail a read of a freed block.
import Base

type L is Data:
  LN{}
  LC{h: U32, t: L}

type Chain is Data:
  End{}
  Link{v: U32, t: Chain}

type Two is Data:
  TZ{}
  TN{a: Chain, b: Chain}

def gen(n: Nat, +k: U32, acc: L) -> L:
  match n:
    case 0n:
      acc
    case 1n+p:
      gen(p, (k + 1 : U32), LC{k, acc})

def mix(xs: L, +s: U32) -> U32:
  match xs:
    case LN{}:
      s
    case LC{h, t}:
      mix(t, ((s * 31 : U32) + h : U32))

def lsum(xs: L, +s: U32) -> U32:
  match xs:
    case LN{}:
      s
    case LC{h, t}:
      lsum(t, (s + h : U32))

def copy(xs: L) -> Chain:
  match xs:
    case LN{}:
      End{}
    case LC{h, t}:
      r = copy(t)
      Link{(h + 1 : U32), r}

def csum(c: Chain, +s: U32) -> U32:
  match c:
    case End{}:
      s
    case Link{v, t}:
      csum(t, ((s * 31 : U32) + v : U32))

def lend(n: Nat) -> U32:
  +xs = gen(n, 1, LN{})
  c = copy(xs)
  (csum(c, 0) + mix(xs, 0) : U32)

def wrap(n: Nat) -> Chain:
  xs = gen(n, 7, LN{})
  t = copy(xs)
  Link{9, t}

def pair(+n: Nat) -> Two:
  xs = gen(n, 3, LN{})
  ys = gen(n, 5, LN{})
  a b = copy(xs) copy(ys)
  TN{a, b}

def tsum(t: Two) -> U32:
  match t:
    case TZ{}:
      0
    case TN{a, b}:
      (csum(a, 0) + csum(b, 1) : U32)

def wrap2(n: Nat) -> Chain:
  +xs = gen(n, 7, LN{})
  t = copy(xs)
  Link{lsum(xs, 0), t}

def main() -> IO(Unit):
  do IO<Unit>:
    Unit <- IO.print(U32.show(lend(1000n)))
    Unit <- IO.print(U32.show(csum(wrap(1000n), 0)))
    Unit <- IO.print(U32.show(tsum(pair(1000n))))
    IO.print(U32.show(csum(wrap2(1000n), 0)))

#|2911871640
#|2084625045
#|378632985
#|507450896
