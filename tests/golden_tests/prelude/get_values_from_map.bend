def test1() -> (u24, Map(u24)):
  m = {}
  m = Map/set(Map/set(Map/set(Map/empty, 3, 4), 2, 3), 1, 2)
  (val, map) = Map/get(m, 1) 
  return Map/get(map, val)
 

def main() -> _:
  return test1()
