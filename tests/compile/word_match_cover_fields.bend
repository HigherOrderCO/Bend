# word_match_cover_fields: a word match's default covers a deeper
# default only when the two are the same term, down to a constructor's
# name and a match's every arm. Here the odd arm and the default differ
# only in a constructor (True{} against False{}) or in a nested match's
# last arm (2 against 3), so neither may fold into the other.
import Base

def id(b: Bool) -> Bool:
  b

def odd(n: U32) -> Bool:
  match n:
    case U32{WCon{True{}, _}}:
      id(True{})
    case _:
      id(False{})

def pick(n: U32, b: Bool) -> U32:
  match n:
    case U32{WCon{True{}, _}}:
      match b:
        case True{}:
          1
        case False{}:
          2
    case _:
      match b:
        case True{}:
          1
        case False{}:
          3

def show(b: Bool, n: U32) -> String:
  String.append(Bool.show(b), String.append(" ", String.append(U32.show(n), " ")))

def main() -> IO(Unit):
  IO.print(String.append(show(odd(1), pick(1, False{})), show(odd(2), pick(2, False{}))))

#|True 2 False 3 
