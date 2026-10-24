# A module's internal ns:name key must differ from a local dotted name,
# both for constructor labels and for members of one recursive group.
import Base
import ./label_namespace_lib.bend as L

type label_namespace_lib.Good is Data:
  label_namespace_lib.K{}

def imported() -> L.Good:
  L.K{}

def local() -> label_namespace_lib.Good:
  label_namespace_lib.K{}

def one(x: L.Good) -> Nat:
  match x:
    case L.K{}: 1n

def two(x: label_namespace_lib.Good) -> Nat:
  match x:
    case label_namespace_lib.K{}: 2n

def keep(n: Nat, -m: Nat) -> Nat:
  n

def label_namespace_lib.k(n: Nat) -> Nat:
  keep(n, L.k(n))

def L.k(n):
  match n:
    case 0n: 0n
    case 1n+p: label_namespace_lib.k(p)

law group_result:
  {L.k(4n) == 3n : Nat}

def group_result():
  {==}

def main() -> Nat:
  Nat.add(L.k(4n), Nat.add(one(imported()), two(local())))

#|6n
