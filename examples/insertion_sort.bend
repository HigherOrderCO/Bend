def insertion_sort(xs):
  match xs:
    case List/Nil:
      return List/Nil
    case List/Cons:
      return insertion_sort.insert(xs.head, insertion_sort(xs.tail))

def insertion_sort.insert(v, xs):
  match xs:
    case List/Nil:
      return List/Cons(v, List/Nil)
    case List/Cons:
      return insert_aux(v > xs.head, v, xs.head, xs.tail)

def insert_aux(n, v, x, xs):
  if n == 0:
    return List/Cons(v, List/Cons(x, xs))
  else:
    return List/Cons(x, insertion_sort.insert(v, xs))

def rnd(n):
  if n == 0:
    return List/Nil
  else:
    return List/Cons(random(10000 - n), rnd(n - 1))

def random(n):
  if n == 0:
    return 0
  else:
    return (random(n - 1) * 16 + 101387) % 429453

def sum(xs):
  match xs:
    case List/Nil:
      return 0
    case List/Cons:
      return xs.head + sum(xs.tail)

def main():
  return sum(insertion_sort(rnd(1000)))
