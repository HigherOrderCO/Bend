def reverse(list):
  fold list with acc = []:
    case List/Nil:
      return acc
    case List/Cons:
      return list.tail(List/Cons(list.head, acc))

def main:
  bend n = 5:
    when n != 0:
      xs = List/Cons(n, fork(n - 1))
    else:
      xs = List/Nil
  return reverse(xs)