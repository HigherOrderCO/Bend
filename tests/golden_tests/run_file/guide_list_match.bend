def main:
  my_list = [1, 2, 3]
  match my_list:
    case List/Cons:
      return my_list.head
    case List/Nil:
      return 0