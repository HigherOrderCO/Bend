# Despite having an unused variable in main, `map(tt,l)` should not be extracted into a new definition.
def map(fn, list):
  fold list:
    case List/Cons:
      return List/Cons(fn(list.head), list.tail)
    case List/Nil:
      return []

def tt(x):
  return x*2

def main():
  l = [5,6,7,8]
  k = [1,2,3,4]
  return map(tt,l)