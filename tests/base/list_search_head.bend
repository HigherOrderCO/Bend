# A list search whose head decides the result reduces over any tail (#1616).
import Base

def all_head(-t: List<Nat>) -> {List.all(~&1, ~Nat, ~(n => Nat.is_le(n, 3n)), 5n <> t) == False{} : Bool}:
  {==}

def any_head(-t: List<Nat>) -> {List.any(~&1, ~Nat, ~(n => Nat.is_le(n, 3n)), 1n <> t) == True{} : Bool}:
  {==}

def find_head(-t: List<&2, Nat>) -> {List.find(~Nat, ~(n => Nat.is_le(n, 3n)), 1n <> t) == Some{1n} : Maybe<&2, Nat>}:
  {==}

def contains_head(-t: List<&2, Nat>) -> {List.contains(~Nat, ~Nat.is_eq, 3n <> t, 3n) == True{} : Bool}:
  {==}

def all_second(-t: List<Nat>) -> {List.all(~&1, ~Nat, ~(n => Nat.is_le(n, 3n)), 1n <> 5n <> t) == False{} : Bool}:
  {==}

def main() -> IO(Unit):
  IO.print(Bool.show(List.all(~&1, ~Nat, ~(n => Nat.is_le(n, 3n)), [5n, 1n])))

#|False
