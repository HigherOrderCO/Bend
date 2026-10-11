import Base

def drop(-n: Nat, w: Word(n)) -> U32:
  match w:
    case rest:
      42

def take(x: F32) -> U32:
  match x:
    case F32{WCon{head, tail}}:
      drop!(31n, tail)

def main() -> U32:
  take(3.5)

#|42
