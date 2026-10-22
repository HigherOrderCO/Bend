# Nat's comparisons run as native C and JS (comp.ts OPERATIONS
# nat_is_lt, and nat_cmp under the rest), not as Base's defs.
# model proves by {==} that the calls equal these values in Base's defs,
# so --verdict pins the model; main prints the same calls, so every lane
# must print what the model computes. The values are each lane's answers.
import Base

def calls() -> List<Bool>:
  [
    Nat.is_eq(0n, 0n),
    Nat.is_eq(0n, 7n),
    Nat.is_eq(7n, 0n),
    Nat.is_eq(63n, 64n),
    Nat.is_eq(64n, 63n),
    Nat.is_eq(64n, 64n),
    Nat.is_ne(0n, 0n),
    Nat.is_ne(0n, 7n),
    Nat.is_ne(7n, 0n),
    Nat.is_ne(63n, 64n),
    Nat.is_ne(64n, 63n),
    Nat.is_ne(64n, 64n),
    Nat.is_lt(0n, 0n),
    Nat.is_lt(0n, 7n),
    Nat.is_lt(7n, 0n),
    Nat.is_lt(63n, 64n),
    Nat.is_lt(64n, 63n),
    Nat.is_lt(64n, 64n),
    Nat.is_le(0n, 0n),
    Nat.is_le(0n, 7n),
    Nat.is_le(7n, 0n),
    Nat.is_le(63n, 64n),
    Nat.is_le(64n, 63n),
    Nat.is_le(64n, 64n),
    Nat.is_gt(0n, 0n),
    Nat.is_gt(0n, 7n),
    Nat.is_gt(7n, 0n),
    Nat.is_gt(63n, 64n),
    Nat.is_gt(64n, 63n),
    Nat.is_gt(64n, 64n),
    Nat.is_ge(0n, 0n),
    Nat.is_ge(0n, 7n),
    Nat.is_ge(7n, 0n),
    Nat.is_ge(63n, 64n),
    Nat.is_ge(64n, 63n),
    Nat.is_ge(64n, 64n)
  ]

def model() -> {calls() == [
    True{},
    False{},
    False{},
    False{},
    False{},
    True{},
    False{},
    True{},
    True{},
    True{},
    True{},
    False{},
    False{},
    True{},
    False{},
    True{},
    False{},
    False{},
    True{},
    True{},
    False{},
    True{},
    False{},
    True{},
    False{},
    False{},
    True{},
    False{},
    True{},
    False{},
    True{},
    False{},
    True{},
    False{},
    True{},
    True{}
  ] : List<Bool>}:
  {==}

def main() -> List<Bool>:
  calls()

#|[True{}, False{}, False{}, False{}, False{}, True{}, False{}, True{}, True{}, True{}, True{}, False{}, False{}, True{}, False{}, True{}, False{}, False{}, True{}, True{}, False{}, True{}, False{}, True{}, False{}, False{}, True{}, False{}, True{}, False{}, True{}, False{}, True{}, False{}, True{}, True{}]
