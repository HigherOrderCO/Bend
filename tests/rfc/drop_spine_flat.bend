# drop order through a flat field: RCon's p is a P2 laid out in RCon's own
# slots, with a phantom Ph<RL> and a List box. Only rest holds an RL, so
# the spine is rest's slot, not the List's: a box counts by the type it
# stores, not by the field's type. The order itself can't be seen here;
# each round checks every field still drops once. Expect 2 rounds of 5050.
import Base

type Ph<-A: Type> is Data:
  Ph{}

type P2<-A: Data, -B: Data> is Data:
  P2{a: A, b: B}

type RL is Data:
  RNil{}
  RCon{rest: RL, p: P2<Ph<RL>, List<&2, U32>>}

def rl(+n: Nat) -> RL:
  match n:
    case 0n:
      RNil{}
    case 1n+m:
      RCon{rl(m), P2{Ph{}, [U32.from_nat(n)]}}

def first(xs: List<&2, U32>) -> U32:
  match xs:
    case []:
      0
    case x <> r:
      x

def rsum(x: RL) -> U32:
  match x:
    case RNil{}:
      0
    case RCon{rest, P2{ph, item}}:
      U32.add(first(item), rsum(rest))

def waste(a: RL, x: U32) -> U32:
  x

def round(+n: Nat) -> U32:
  U32.add(waste(rl(n), 0), rsum(rl(n)))

def main() -> IO(Unit):
  IO.print(Nat.show(U32.to_nat(round(100n))) ++ " " ++ Nat.show(U32.to_nat(round(100n))))

#|5050 5050
