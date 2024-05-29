def main:
  x = 1
  x @= lambda x: x + 1
  map = { 0: 3, 1: 4 }
  map[1] += 1
  map[1] @= lambda x: x * 2
  return (x, map[1], map[0])
