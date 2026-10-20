# a family whose index kind holds the family itself: working out which
# of T's parameters are values walks D, which meets T again before its
# answer is in, and reads its arguments as values instead of looping
import Base

type D is Data:
  DNil{}
  DCons{t: T<DNil{}>}

type T<-d: D> is Data:
  TNil{}
  TOne{v: U32}

def get(t: T<DNil{}>) -> U32:
  match t:
    case TNil{}:
      0
    case TOne{v}:
      v

def main() -> U32:
  get(TOne{5})

#|5
