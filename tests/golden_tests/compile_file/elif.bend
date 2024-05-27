def main:
  cond1 = 1 == 2
  cond2 = 2 < 1
  cond3 = 3 > 2
  cond4 = 2 == 2
  if cond1:
    res = 1
  elif cond2:
    res = 2
  elif cond3:
    res = 3
  elif cond4:
    res = 4
  else:
    res = 0
  return res
