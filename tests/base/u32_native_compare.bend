# U32's comparisons run as native C and JS (comp.ts OPERATIONS),
# not as Base's defs.
# model proves by {==} that the calls equal these values in Base's defs,
# so --verdict pins the model; main prints the same calls, so every lane
# must print what the model computes. The values are each lane's answers.
import Base

def calls() -> List<Bool>:
  [
    U32.is_eq(0, 0),
    U32.is_eq(0, 4294967295),
    U32.is_eq(4294967295, 0),
    U32.is_eq(2147483648, 2147483647),
    U32.is_eq(2147483647, 2147483648),
    U32.is_eq(4294967295, 4294967295),
    U32.is_ne(0, 0),
    U32.is_ne(0, 4294967295),
    U32.is_ne(4294967295, 0),
    U32.is_ne(2147483648, 2147483647),
    U32.is_ne(2147483647, 2147483648),
    U32.is_ne(4294967295, 4294967295),
    U32.is_lt(0, 0),
    U32.is_lt(0, 4294967295),
    U32.is_lt(4294967295, 0),
    U32.is_lt(2147483648, 2147483647),
    U32.is_lt(2147483647, 2147483648),
    U32.is_lt(4294967295, 4294967295),
    U32.is_le(0, 0),
    U32.is_le(0, 4294967295),
    U32.is_le(4294967295, 0),
    U32.is_le(2147483648, 2147483647),
    U32.is_le(2147483647, 2147483648),
    U32.is_le(4294967295, 4294967295),
    U32.is_gt(0, 0),
    U32.is_gt(0, 4294967295),
    U32.is_gt(4294967295, 0),
    U32.is_gt(2147483648, 2147483647),
    U32.is_gt(2147483647, 2147483648),
    U32.is_gt(4294967295, 4294967295),
    U32.is_ge(0, 0),
    U32.is_ge(0, 4294967295),
    U32.is_ge(4294967295, 0),
    U32.is_ge(2147483648, 2147483647),
    U32.is_ge(2147483647, 2147483648),
    U32.is_ge(4294967295, 4294967295),
    U32.is_zero(0),
    U32.is_zero(1),
    U32.is_zero(2147483648),
    U32.is_zero(4294967295)
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
    False{},
    True{},
    False{},
    True{},
    True{},
    False{},
    False{},
    True{},
    True{},
    False{},
    False{},
    True{},
    True{},
    False{},
    False{},
    True{},
    False{},
    True{},
    True{},
    False{},
    True{},
    True{},
    False{},
    False{},
    False{}
  ] : List<Bool>}:
  {==}

def main() -> List<Bool>:
  calls()

#|[True{}, False{}, False{}, False{}, False{}, True{}, False{}, True{}, True{}, True{}, True{}, False{}, False{}, True{}, False{}, False{}, True{}, False{}, True{}, True{}, False{}, False{}, True{}, True{}, False{}, False{}, True{}, True{}, False{}, False{}, True{}, False{}, True{}, True{}, False{}, True{}, True{}, False{}, False{}, False{}]
