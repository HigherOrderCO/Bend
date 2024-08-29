# Sorts a list
def sort(xs: List(u24)) -> List(u24):
  match xs:
    case List/Nil: 
      return List/Nil
    case List/Cons: 
      return insert(xs.head, sort(xs.tail))

def insert(v: u24, xs: List(u24)) -> List(u24):
  match xs:
    case List/Nil:
      return List/Cons(v, List/Nil)
    case List/Cons:
      return swap_gt(v, xs.head, xs.tail)

def swap_gt(v: u24, x: u24, xs: List(u24)) -> List(u24):
  if x > v:
    return List/Cons(v, List/Cons(x, xs))
  else:
    return List/Cons(x, insert(v, xs))

# Generates a list of 'n' random u24 numbers using xorshift
def rnd(n: u24) -> List(u24):
  bend n, state=1:
    when n != 0:
      state = state ^ (state << 13)
      state = state ^ (state >> 17)
      state = state ^ (state << 5)
      return List/Cons(state % 100, fork(n - 1, state))
    else:
      return List/Nil

# Sums a list of u24 numbers
def sum(xs: List(u24)) -> u24:
  fold xs:
    case List/Nil:
      return 0
    case List/Cons:
      return xs.head + xs.tail

def main() -> u24:
  n = 100
  return sum(sort(rnd(n)))
