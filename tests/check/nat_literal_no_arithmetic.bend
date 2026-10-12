type Nat is Data:
  Zero{}
  Succ{pred: Nat}

def id(~n: Nat) -> Nat:
  n

def main() -> Nat:
  id(~4097n)

def claim() -> {id(~4097n) == 4097n : Nat}:
  {==}

#|4097n
