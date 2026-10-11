# A type-only template can close over run-time Data. Its kernel instance
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

#|ALL PROOFS CHECK
