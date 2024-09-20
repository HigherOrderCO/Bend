def insertion_sort(xs: List(u24)) -> List(u24):
  match xs:
    case List/Nil:
      return List/Nil
    case List/Cons:
      return insertion_sort.insert(xs.head, insertion_sort(xs.tail))

# Inserts a value into a partially sorted list
def insertion_sort.insert(v: u24, xs: List(u24)) -> List(u24):
  match xs:
    case List/Nil:
      return List/Cons(v, List/Nil)
    case List/Cons:
      return insert_aux(v > xs.head, v, xs.head, xs.tail)

def insert_aux(n: u24, v: u24, x: u24, xs: List(u24)) -> List(u24):
  if n == 0:
    return List/Cons(v, List/Cons(x, xs))
  else:
    return List/Cons(x, insertion_sort.insert(v, xs))

# Generates a list of random numbers
def rnd(n: u24) -> List(u24):
  if n == 0:
    return List/Nil
  else:
    return List/Cons(random(10000 - n), rnd(n - 1))

# Generates a pseudo-random number (not very good)
def random(n: u24) -> u24:
  if n == 0:
    return 0
  else:
    return (random(n - 1) * 16 + 101387) % 429453

def main() -> List(u24):
  return insertion_sort(rnd(10))
