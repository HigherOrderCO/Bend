def main:
  y = 1
  fold x = [0, 0, 0] with y:
    case List/Cons:
      return List/Cons(x.head + y, x.tail(y + 1))
    case List/Nil:
      return List/Nil
