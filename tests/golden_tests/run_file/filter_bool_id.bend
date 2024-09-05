type Bool:
  T
  F

def filter(f, ls):
  match ls:
    case List/Nil:
      return List/Nil
    case List/Cons:
      match f(ls.head):
        case Bool/T:
          return List/Cons(ls.head, filter(f, ls.tail))
        case Bool/F:
          return filter(f, ls.tail)

def main:
  return filter(lambda x: x, [Bool/T])
