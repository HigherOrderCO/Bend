import Base

def drop(-n: Nat, w: Word(n)) -> U32:
  match w:
    case rest:
      42

def take(x: U32, y: U32) -> U32:
  match x:
    case U32{WCon{head, tail}}:
      match y:
        case U32{WCon{other, rest}}:
          U32.add(drop!(31n, tail), drop!(31n, rest))

def main() -> U32:
  take(123, 456)

#|84
