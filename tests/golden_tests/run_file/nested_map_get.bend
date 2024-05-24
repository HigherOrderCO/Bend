def x:
  return { 0: 1, 1: 42, 2: 0, 3: 2 }

def main():
  return x[x[0]] + x[x[x[3]]]
