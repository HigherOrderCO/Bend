def main():
  bend x = 0:
    when x < 3:
      tree = ![fork(x + 1), fork(x + 1)]
    else:
      tree = !7
  return tree