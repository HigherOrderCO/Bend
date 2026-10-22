# U32.cmp and Nat.cmp run as native C and JS (comp.ts OPERATIONS
# u32_cmp, nat_cmp), not as Base's defs.
# model proves by {==} that the calls equal these values in Base's defs,
# so --verdict pins the model; main prints the same calls, so every lane
# must print what the model computes. The values are each lane's answers.
import Base

def calls() -> List<Cmp>:
  [
    U32.cmp(0, 0),
    U32.cmp(0, 4294967295),
    U32.cmp(4294967295, 0),
    U32.cmp(2147483648, 2147483647),
    U32.cmp(2147483647, 2147483648),
    U32.cmp(4294967295, 4294967295),
    Nat.cmp(0n, 0n),
    Nat.cmp(0n, 7n),
    Nat.cmp(7n, 0n),
    Nat.cmp(63n, 64n),
    Nat.cmp(64n, 63n),
    Nat.cmp(64n, 64n)
  ]

def model() -> {calls() == [
    EQ{},
    LT{},
    GT{},
    GT{},
    LT{},
    EQ{},
    EQ{},
    LT{},
    GT{},
    LT{},
    GT{},
    EQ{}
  ] : List<Cmp>}:
  {==}

def main() -> List<Cmp>:
  calls()

#|[EQ{}, LT{}, GT{}, GT{}, LT{}, EQ{}, EQ{}, LT{}, GT{}, LT{}, GT{}, EQ{}]
