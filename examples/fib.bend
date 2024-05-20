def fib(n):
  switch n:
    case 0:
      return 1
    case _:
      p = n - 1
      switch p:
        case 0:
          return 1
        case _:
          return fib(p) + fib(p - 1)

def main():
  return fib(30)
