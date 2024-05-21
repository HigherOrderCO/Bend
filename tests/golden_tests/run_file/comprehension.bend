def inc_list(list):
  return [x + 1 for x in list]

def filter_gt(list, n):
  return [x for x in list if x > n]

def main:
  list = [3, 4, 1, 9, 6, 5]
  list = inc_list(list)
  list = filter_gt(list, 4)
  return list
