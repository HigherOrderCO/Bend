# The next predicate would overflow, but the head already decides each search.
import Base

def pred(n: Nat) -> Bool:
  Nat.is_le(Nat.mul(n, 65537n), 3n)

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print(Bool.show(List.any(~&2, ~Nat, ~pred, [0n, 4294967295n])))
    IO.print(Bool.show(List.all(~&2, ~Nat, ~(n => Bool.not(pred(n))), [0n, 4294967295n])))
    IO.print(Bool.show(List.contains(~Nat, ~(a => b => Nat.is_eq(Nat.mul(a, 65537n), b)), [0n, 4294967295n], 0n)))
    IO.print(Maybe.show(~&2, ~Nat, ~Nat.show, List.find(~Nat, ~pred, [0n, 4294967295n])))

#|True
#|False
#|True
#|Some(0)
