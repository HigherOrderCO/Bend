def main:
  y = 1
  fold x = [0 0 0] with y:
    case List/cons:
      return List/cons(x.head + y x.tail(y + 1))
    case List/nil:
      return List/nil
