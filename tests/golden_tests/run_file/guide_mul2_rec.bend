def slow_mul2(n):
  switch n:
    case 0:
      return 0
    case _:
      return 2 + slow_mul2(n-1)

def main:
  return slow_mul2(7)