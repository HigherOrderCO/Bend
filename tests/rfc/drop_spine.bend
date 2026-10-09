# drop order: term_drop moves a constructor's recursive field to its
# last slot, so its frame frees before the spine drops. RCon has the
# spine first, MCon before a word, and Rose behind a List; each item is
# a boxed one-cell List. Each round drops a fresh owned copy whole,
# then shares one, sums it through one holder and drops it through the
# other; a misplaced field would read back wrong from reused memory in
# a later round. Expect 3 rounds of 3 * 5050 + 100 * 7.
import Base

type RL is Data:
  RNil{}
  RCon{rest: RL, item: List<&2, U32>}

type ML is Data:
  MNil{}
  MCon{item: List<&2, U32>, rest: ML, tag: U32}

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

def rose(+n: Nat) -> Rose:
  match n:
    case 0n:
      Rose{[], [0]}
    case 1n+m:
      Rose{[rose(m)], [U32.from_nat(n)]}

def first(xs: List<&2, U32>) -> U32:
  match xs:
    case []:
      0
    case x <> rest:
      x

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

def rose_sum(r: Rose) -> U32:
  match r:
    case Rose{[], item}:
      first(item)
    case Rose{k <> rest, item}:
      U32.add(first(item), rose_sum(k))

def waste(r: RL, m: ML, o: Rose, x: U32) -> U32:
  x

def round(+n: Nat) -> U32:
  a = waste(rl(n), ml(n), rose(n), 0)
  +r = rl(n)
  +m = ml(n)
  +o = rose(n)
  b = U32.add(rl_sum(r), U32.add(ml_sum(m), rose_sum(o)))
  U32.add(a, waste(r, m, o, b))

def main() -> IO(Unit):
  IO.print(Nat.show(U32.to_nat(round(100n)))
    ++ " " ++ Nat.show(U32.to_nat(round(100n)))
    ++ " " ++ Nat.show(U32.to_nat(round(100n))))

#|15850 15850 15850
