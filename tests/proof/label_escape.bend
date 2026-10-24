# Raw namespace separators and punctuation are escaped as code points;
# source underscores must not imitate those constructor or group labels.
import Base
import ./label-group/lib.bend as L

type label_2d_group_2f_lib_3a_Good is Data:
  label_2d_group_2f_lib_3a_K{}

def imported() -> L.Good:
  L.K{}

def local() -> label_2d_group_2f_lib_3a_Good:
  label_2d_group_2f_lib_3a_K{}

def one(x: L.Good) -> Nat:
  match x:
    case L.K{}: 1n

def two(x: label_2d_group_2f_lib_3a_Good) -> Nat:
  match x:
    case label_2d_group_2f_lib_3a_K{}: 2n

def keep(n: Nat, -m: Nat) -> Nat:
  n

def label_2d_group_2f_lib_3a_k(n: Nat) -> Nat:
  keep(n, L.k(n))

def L.k(n):
  match n:
    case 0n: 0n
    case 1n+p: label_2d_group_2f_lib_3a_k(p)

law group_result:
  {L.k(4n) == 3n : Nat}

def group_result():
  {==}

def main() -> Nat:
  Nat.add(L.k(4n), Nat.add(one(imported()), two(local())))

#|6n
