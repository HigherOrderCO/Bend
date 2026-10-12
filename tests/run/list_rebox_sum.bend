# #970: runtime Scalar and Pair lists exercise boxing across a sum. After
# warm-up, native arena growth is bounded by one allocation quantum,
# including retained free lists. JS checks the values without a heap pin.
import Base

type Value is Data:
  Scalar{number: U32}
  Pair{left: Nat, right: Nat}

def wrap(n: Nat) -> Value:
  match n:
    case 0n:
      Scalar{7}
    case 1n+p:
      Pair{p, 4n}

def rebox(values: List<&2, Value>) -> List<&2, Value>:
  match values:
    case Nil{}:
      Nil{}
    case value <> tail:
      value <> tail

def head_u32(values: List<&2, Value>) -> U32:
  match values:
    case Nil{}:
      0
    case value <> tail:
      match value:
        case Scalar{+n}:
          n
        case Pair{left, right}:
          0

def head_nat(values: List<&2, Value>) -> Nat:
  match values:
    case Nil{}:
      0n
    case value <> tail:
      match value:
        case Scalar{number}:
          0n
        case Pair{+left, right}:
          left

def cycle(n: Nat, +seed: Nat, acc: U32) -> U32:
  match n:
    case 0n:
      acc
    case 1n+more:
      cycle(more, seed, U32.add(acc,
        U32.add(head_u32(rebox(wrap(seed) <> Nil{})),
          U32.from_nat(head_nat(rebox(wrap(Nat.add(seed, 4n)) <> Nil{}))))))

def heap.check(value: U32, expected: U32, before: U32) -> IO(U32):
  import "./list_rebox_sum.c"
  import "./list_rebox_sum.js"

def main() -> IO(Unit):
  do IO<Unit>:
    xs : List<String> <- IO.args()
    +n : Nat = Nat.sub(List.length(&1, String, xs), 1n)
    Unit <- IO.print(U32.show(head_u32(rebox(wrap(n) <> Nil{}))))
    Unit <- IO.print(Nat.show(head_nat(rebox(wrap(Nat.add(n, 4n)) <> Nil{}))))
    before : U32 <- heap.check(cycle(10000n, n, 0), 100000, 0)
    after : U32 <- heap.check(cycle(200000n, n, 0), 2000000, before)
    IO.print("bounded")

#|7
#|3
#|bounded
