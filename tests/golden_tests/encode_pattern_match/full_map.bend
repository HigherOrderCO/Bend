def prng(state):
  state = state ^ (state << 13)
  state = state ^ (state >> 17)
  state = state ^ (state << 5)
  return state

def fullMap:
  bend i = 14:
    when i > 0:
      return Map/Node(1, fork(i - 1), fork(i - 1))
    else:
      return Map/Leaf

def test(map):
  bend i = 0, map:
    when i < 1000:
      (curr, map) = Map/get(map, prng(i) % 4096)
      return curr+fork(i + 1, map)
    else:
      return 0

def main:
  return test(fullMap)
