# List searches find the first match and run on long lists in constant stack.
import Base

def zeros(n: Nat, acc: List<&2, U32>) -> List<&2, U32>:
  match n:
    case 0n:
      acc
    case 1n+p:
      zeros(p, 0 <> acc)

def is_hit(x: U32) -> Bool:
  U32.is_eq(x, 42)

def is_clear(x: U32) -> Bool:
  U32.is_ne(x, 42)

def show(m: Maybe<&2, U32>) -> String:
  Maybe.show(~&2, ~U32, ~U32.show, m)

def apply(f: U32 -> Bool) -> Bool:
  f(7)

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print(Bool.show(List.any(~&2, ~U32, ~is_hit, [])))
    IO.print(Bool.show(List.all(~&2, ~U32, ~is_clear, [])))
    IO.print(show(List.find(~U32, ~is_hit, [])))
    IO.print(Bool.show(List.contains(~U32, ~U32.is_eq, [], 42)))
    IO.print(show(List.find(~U32, ~U32.is_even, [1, 2, 4])))
    IO.print(Bool.show(List.any(~&1, ~(U32 -> Bool), ~apply,
      [x => U32.is_eq(x, 7), x => U32.is_eq(x, 8)])))
    IO.print(Bool.show(List.all(~&1, ~(U32 -> Bool), ~apply,
      [x => U32.is_eq(x, 8), x => U32.is_eq(x, 7)])))
    +xs : List<&2, U32> = zeros(50000n, [])
    IO.print(Bool.show(List.any(~&2, ~U32, ~is_hit, xs)))
    IO.print(Bool.show(List.all(~&2, ~U32, ~is_clear, xs)))
    IO.print(show(List.find(~U32, ~is_hit, xs)))
    IO.print(Bool.show(List.contains(~U32, ~U32.is_eq, xs, 42)))
    +last : List<&2, U32> = zeros(50000n, [42])
    IO.print(Bool.show(List.any(~&2, ~U32, ~is_hit, last)))
    IO.print(Bool.show(List.all(~&2, ~U32, ~is_clear, last)))
    IO.print(show(List.find(~U32, ~is_hit, last)))
    IO.print(Bool.show(List.contains(~U32, ~U32.is_eq, last, 42)))

#|False
#|True
#|None
#|False
#|Some(2)
#|True
#|False
#|False
#|True
#|None
#|False
#|True
#|False
#|Some(42)
#|True
