type Nat is Data:
  Zero{}
  Succ{pred: Nat}

type Bool is Data:
  False{}
  True{}

def Nat.mul(a: Nat, b: Nat) -> Nat:
  a

def Nat.add(a: Nat, b: Nat) -> Nat:
  a

def id(~n: Nat) -> Nat:
  n

def two_succ(n: Nat) -> Bool:
  match n:
    case Zero{}:
      False{}
    case Succ{p}:
      match p:
        case Zero{}:
          False{}
        case Succ{q}:
          True{}

def main() -> Nat:
  id(~4097n)

# construct 2^depth successors without a large literal or Nat arithmetic
def grow(+depth: Nat, n: Nat) -> Nat:
  match depth:
    case Zero{}:
      Succ{n}
    case Succ{p}:
      grow(p, grow(p, n))

def claim() -> {id(~4097n) == Succ{grow(12n, Zero{})} : Nat}:
  {==}

def claim_two() -> {two_succ(id(~4097n)) == True{} : Bool}:
  {==}

def claim_one() -> {two_succ(id(~1n)) == False{} : Bool}:
  {==}

#|4097n
