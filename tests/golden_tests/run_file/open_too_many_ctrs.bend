type MyTree:
  node { a, b }
  leaf { a }

def main:
  x = MyTree/node(1, 2)
  open MyTree: x
  return x.a