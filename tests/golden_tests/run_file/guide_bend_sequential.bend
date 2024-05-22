def foo(_):
  bend idx = 0:
    when idx < 10:
      sum = idx + fork(idx + 1)
    else:
      sum = 0
  return sum

def main:
  return foo(*)