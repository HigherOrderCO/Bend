def is_even(n):
  if n % 2 == 0:
    return 1
  else:
    return 0

def get_even_sum_under_if(this_number):
  bend current = 0:
    when current < this_number:
      new_num = fork(current + is_even(current))

      return new_num

def main():
  return get_even_sum_under_if(1000)
