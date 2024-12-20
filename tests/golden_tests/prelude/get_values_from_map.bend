def test1() -> (u24):
  m = {}
  m = Map/set(Map/set(Map/set(Map/empty, 3, 4), 2, 3), 1, 2)
  (val1, map1) = Map/get(m, 1) 
  (val2, map2) = Map/get(map1, val1)
  return val2

def main() -> _:
  return test1()
