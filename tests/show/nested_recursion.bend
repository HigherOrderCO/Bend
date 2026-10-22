# a pure main prints types that hold each other: a Rose's printer and its
# List's call each other, as do A's and B's (was a dead claim at the
# printer below), and a def named like a printer local stays the user's
import Base

type Rose is Data:
  Rose{v: U32, kids: List<&2, Rose>}

type A is Data:
  A0{}
  A1{b: B}

type B is Data:
  B1{a: A}

type Box<n: Nat> is Data:
  Box{v: U32}

def x() -> Nat:
  2n

def main() -> Rose & A & Array<Box<x()>>:
  (Rose{1, [Rose{2, []}, Rose{3, [Rose{4, []}]}]}, A1{B1{A1{B1{A0{}}}}},
    Array.new(Box<x()>, 1n, Box{3}))

#|(Rose{1, [Rose{2, []}, Rose{3, [Rose{4, []}]}]}, A1{B1{A1{B1{A0{}}}}}, [Box{3}, Box{3}])
