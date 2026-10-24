# A constructor tail that boxes a call's result once is built before that
# call, which fills the field. mid fills a box between word fields; even
# and odd pass one destination around their cycle; map rebuilds each cell
# in place; right builds a three-way fork's node before its calls, which
# fill it in turn; dup holds its result twice, so it builds after the call
# as before.
import Base

type Mid is Data:
  Mid.End{}
  Mid.Node{a: U32, t: Mid, b: Bool, c: U32}

def mid(+n: Nat, +i: U32) -> Mid:
  match n:
    case 0n:
      Mid.End{}
    case 1n+p:
      t = mid(p, (i + 1 : U32))
      Mid.Node{i, t, True{}, (i * 3 : U32)}

def mid.sum(m: Mid, +s: U32) -> U32:
  match m:
    case Mid.End{}:
      s
    case Mid.Node{a, t, b, c}:
      mid.sum(t, (((s * 31 : U32) + a : U32) + c : U32))

type Chain is Data:
  End{}
  A{t: Chain}
  B{t: Chain}

@unsafe
def even(n: Nat) -> Chain:
  match n:
    case 0n:
      End{}
    case 1n+p:
      t = odd(p)
      A{t}

@unsafe
def odd(n: Nat) -> Chain:
  match n:
    case 0n:
      End{}
    case 1n+p:
      t = even(p)
      B{t}

def score(c: Chain, +s: U32) -> U32:
  match c:
    case End{}:
      s
    case A{t}:
      score(t, ((s * 3 : U32) + 1 : U32))
    case B{t}:
      score(t, ((s * 3 : U32) + 2 : U32))

def gen(n: Nat, +k: U32, acc: List<U32>) -> List<U32>:
  match n:
    case 0n:
      acc
    case 1n+p:
      gen(p, (k + 1 : U32), k <> acc)

def mix(xs: List<U32>, +s: U32) -> U32:
  match xs:
    case []:
      s
    case h <> t:
      mix(t, ((s * 31 : U32) + h : U32))

type T3 is Data:
  Lf{v: U32}
  N3{a: T3, b: T3, c: T3}

def leaf(+i: U32) -> T3:
  Lf{(i + 2 : U32)}

def right(n: Nat, +i: U32) -> T3:
  match n:
    case 0n:
      Lf{i}
    case 1n+p:
      a b c = leaf(i) leaf((i * 7 : U32)) right(p, (i + 1 : U32))
      N3{a, b, c}

def t3.sum(t: T3) -> U32:
  match t:
    case Lf{v}:
      v
    case N3{a, b, c}:
      x y z = t3.sum(a) t3.sum(b) t3.sum(c)
      ((x * 3 : U32) + ((y * 5 : U32) + z : U32) : U32)

type Two is Data:
  Z{}
  Two{a: Two, b: Two}

def dup(+n: Nat) -> Two:
  match n:
    case 0n:
      Z{}
    case 1n+p:
      +t = dup(p)
      Two{t, t}

def size(t: Two) -> Nat:
  match t:
    case Z{}:
      1n
    case Two{a, b}:
      Nat.add(size(a), size(b))

def main() -> IO(Unit):
  do IO<Unit>:
    Unit <- IO.print(U32.show(mid.sum(mid(3000n, 5), 0)))
    Unit <- IO.print(U32.show(score(even(3001n), 0)))
    xs : List<U32> = List.map(~U32, ~U32, ~(x => (x * 2 : U32)), gen(3000n, 1, []))
    Unit <- IO.print(U32.show(mix(xs, 0)))
    Unit <- IO.print(U32.show(t3.sum(right(2000n, 1))))
    IO.print(Nat.show(size(dup(10n))))

#|2944146800
#|240525077
#|4041911368
#|76072001
#|1024
