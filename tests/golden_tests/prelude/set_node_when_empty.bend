# Sets a value if the given node is empty, otherwise returns the map
def test(m: Map(T), x: u24, v: T) -> Map(T):
  (val, map) = Map/get_check(m, x)
  match val:
    case Maybe/Some:
      return m
    case Maybe/None:
      return Map/set(m, x, v)

def main() -> _:
  m = {0: 42, 1:23} 
  return test(m, 3, 4)
