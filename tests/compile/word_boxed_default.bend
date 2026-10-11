import Base

def drop(-n: Nat, w: Word(n)) -> U32:
  match w:
    case rest:
      42

def take(x: U32) -> U32:
  match x:
    case U32{WCon{head, tail}}:
      drop!(31n, tail)

def main() -> U32:
  take(123)

#|42
