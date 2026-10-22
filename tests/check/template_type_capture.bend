# A type-only template can close over a typed run-time value. Its kernel instance
# must take the actual captures, not freeze them as global constants.
import Base

def least(lo: Nat, xs: List<&2, Nat>) -> Data:
  {List.all(~&2, ~Nat, ~(y => Nat.is_le(lo, y)), xs) == True{} : Bool}

def least_nil(lo: Nat) -> least(lo, Nil{}):
  {==}

def below(lo: Nat, xs: List<&2, Nat>) -> Data:
  {List.all(~&2, ~Nat, ~(y => Nat.is_le(lo, y)), xs) == False{} : Bool}

def below_one() -> below(2n, [1n]):
  {==}

def bounded(lo: Nat, hi: Nat, xs: List<&2, Nat>) -> Data:
  {List.all(~&2, ~Nat, ~(y => Bool.and(Nat.is_le(lo, y), Nat.is_le(y, hi))), xs) == True{} : Bool}

def bounded_nil(lo: Nat, hi: Nat) -> bounded(lo, hi, Nil{}):
  {==}

def bounded_two() -> bounded(2n, 3n, [2n, 3n]):
  {==}

def outside(lo: Nat, hi: Nat, xs: List<&2, Nat>) -> Data:
  {List.all(~&2, ~Nat, ~(y => Bool.and(Nat.is_le(lo, y), Nat.is_le(y, hi))), xs) == False{} : Bool}

def above_three() -> outside(2n, 3n, [4n]):
  {==}

def shifted(+unused: Nat, lo: Nat, xs: List<&2, Nat>) -> Data:
  {List.all(~&2, ~Nat, ~(y => Nat.is_le(lo, y)), xs) == False{} : Bool}

def shifted_one() -> shifted(99n, 2n, [1n]):
  {==}

type Bounds is Data:
  Bounds{lo: Nat, hi: Nat}

def allows(b: Bounds, +y: Nat) -> Bool:
  Bounds{lo, hi} = b
  Bool.and(Nat.is_le(lo, y), Nat.is_le(y, hi))

def record_bound(b: Bounds, xs: List<&2, Nat>) -> Data:
  {List.all(~&2, ~Nat, ~(y => allows(b, y)), xs) == True{} : Bool}

def record_two() -> record_bound(Bounds{2n, 3n}, [2n, 3n]):
  {==}

# A lifted predicate's outer lo and T's local hi have different bindings,
# even though both have frontend level zero in their own definitions.
def T(~f: Nat -> Bool, hi: Nat, xs: List<&2, Nat>) -> Data:
  {List.all(~&2, ~Nat, ~(y => Bool.and(f(y), Nat.is_le(y, hi))), xs) == True{} : Bool}

def bounded_zero(lo: Nat, p: {True{} == Nat.is_le(lo, 0n) : Bool}) -> T(~(y => Nat.is_le(lo, y)), 0n, [0n]):
  %e@p : {List.all(~&2, ~Nat, ~(y => Bool.and(_, Nat.is_le(y, 0n))), [0n]) == True{} : Bool}
  {==}

law refl_all:
  for ~b: Bool
  for xs: List<&2, Bool>
  {List.all(~&2, ~Bool, ~(y => Bool.and(y, b)), xs) == List.all(~&2, ~Bool, ~(y => Bool.and(y, b)), xs) : Bool}

def refl_all(b, xs):
  {==}

#|ALL PROOFS CHECK
#|Use --verdict for mathematical validity.
