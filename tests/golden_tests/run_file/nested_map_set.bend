def main():
  map = { 0: 1, 1: 10, 2: 0, 3: 1, 4: 3 }
  map[map[0]] = 99
  map[map[2]] = 1
  return map[map[map[4]]] + map[map[2]]
