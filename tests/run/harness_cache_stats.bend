# gates/test.ts's state_run compiles a second checked copy of this book
# in one process, C and JS over and over, around normalization and the
# next fixture: each must give the first copy's bytes.
import Base

law flip:
  for c: Cmp
  Cmp

def flip(c):
  match c:
    case LT{}:
      GT{}
    case EQ{}:
      EQ{}
    case GT{}:
      LT{}

def Cmp.text(c: Cmp) -> String:
  match c:
    case LT{}:
      "LT{}"
    case EQ{}:
      "EQ{}"
    case GT{}:
      "GT{}"

law main:
  IO(Unit)

def main():
  IO.print(Cmp.text(flip(flip(LT{}))))

#|LT{}
