# Checks if a generic map contains a given key, and if it does, applies a function to the value, otherwise it returns the map
def test(m: Map(u24), key: u24) -> Map(u24):
  def addtwo (x: u24) -> u24:
    return (x + 2)
  (num, map) = Map/contains(m, key)
  if (num == 0):
    return m
  else:
    return Map/map(m, key, addtwo())

def main() -> _:
  m = {3: 255}
  return test(m, 3)
