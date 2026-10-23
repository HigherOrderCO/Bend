# drop order: term_drop moves a constructor's recursive field to its
# last slot and frees a frame once only trivial slots follow, so the
# frame frees before the spine drops. RCon has the spine first, MCon
# before a word, Rose behind a List and OCon behind a flat Maybe's tag;
# each item is a boxed one-cell List. Each round drops a fresh owned copy
# whole, then shares one, sums it through one holder and drops it through
# the other; a misplaced field would read back wrong from reused memory
# in a later round. head only reads a word, so it borrows; first passes
# its item through give, which owns it, so each sum owns its copy. Build
# with CCC_OVERRIDE_OPTIONS=+-DBEND_POISON to also fail a read of a freed
# block within one drop. Expect 3 rounds of 4 * 5050 + 100 * 7.
import Base

type RL is Data:
  RNil{}
  RCon{rest: RL, item: List<&2, U32>}

type ML is Data:
  MNil{}
  MCon{item: List<&2, U32>, rest: ML, tag: U32}

type OL is Data:
  ONil{}
  OCon{next: Maybe<&2, OL>, item: List<&2, U32>}

type Rose is Data:
  Rose{kids: List<&2, Rose>, item: List<&2, U32>}

def rl(+n: Nat) -> RL:
  match n:
    case 0n:
      RNil{}
    case 1n+m:
      RCon{rl(m), [U32.from_nat(n)]}

def ml(+n: Nat) -> ML:
  match n:
    case 0n:
      MNil{}
    case 1n+m:
      MCon{[U32.from_nat(n)], ml(m), 7}

def ol(+n: Nat) -> OL:
  match n:
    case 0n:
      ONil{}
    case 1n+m:
      OCon{Some{ol(m)}, [U32.from_nat(n)]}

def rose(+n: Nat) -> Rose:
  match n:
    case 0n:
      Rose{[], [0]}
    case 1n+m:
      Rose{[rose(m)], [U32.from_nat(n)]}

def head(xs: List<&2, U32>) -> U32:
  match xs:
    case []:
      0
    case x <> rest:
      x

def give(xs: List<&2, U32>) -> List<&2, U32>:
  xs

def first(xs: List<&2, U32>) -> U32:
  head(give(xs))

def rl_sum(xs: RL) -> U32:
  match xs:
    case RNil{}:
      0
    case RCon{rest, item}:
      U32.add(first(item), rl_sum(rest))

def ml_sum(xs: ML) -> U32:
  match xs:
    case MNil{}:
      0
    case MCon{item, rest, tag}:
      U32.add(U32.add(first(item), tag), ml_sum(rest))

def ol_sum(xs: OL) -> U32:
  match xs:
    case ONil{}:
      0
    case OCon{None{}, item}:
      first(item)
    case OCon{Some{rest}, item}:
      U32.add(first(item), ol_sum(rest))

def rose_sum(r: Rose) -> U32:
  match r:
    case Rose{[], item}:
      first(item)
    case Rose{k <> rest, item}:
      U32.add(first(item), rose_sum(k))

def waste(r: RL, m: ML, o: Rose, l: OL, x: U32) -> U32:
  x

def round(+n: Nat) -> U32:
  a = waste(rl(n), ml(n), rose(n), ol(n), 0)
  +r = rl(n)
  +m = ml(n)
  +o = rose(n)
  +l = ol(n)
  b = U32.add(U32.add(rl_sum(r), ml_sum(m)), U32.add(rose_sum(o), ol_sum(l)))
  U32.add(a, waste(r, m, o, l, b))

def main() -> IO(Unit):
  IO.print(Nat.show(U32.to_nat(round(100n)))
    ++ " " ++ Nat.show(U32.to_nat(round(100n)))
    ++ " " ++ Nat.show(U32.to_nat(round(100n))))

#|20900 20900 20900
