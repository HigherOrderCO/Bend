def main():
  bend d = 0, i = 0:
    when d < 15: # in the guide its 28, but that takes way too long
      sum = fork(d+1, i*2+0) + fork(d+1, i*2+1)
    else:
      sum = i
  return sum