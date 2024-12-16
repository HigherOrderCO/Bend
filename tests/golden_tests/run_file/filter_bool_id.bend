type Bool = True | False

def filter(f, ls):
  match ls:
    case List/Nil:
      return List/Nil
    case List/Cons:
      match f(ls.head):
        case Bool/True:
          return List/Cons(ls.head, filter(f, ls.tail))
        case Bool/False:
          return filter(f, ls.tail)

def main:
  return filter(lambda x: x, [Bool/True])
