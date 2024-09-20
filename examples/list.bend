##############################
# Author: Ematth, 2024
##############################
### Singly-Linked List Type Definition: ###

# The List type is builtin, so we don't need to declare it.
# But this is how it's defined in the builtins file.
# type List:
#   Nil
#   Cons { head, ~tail }

###########################################

# List clear:
# clears all elements from list l. This is equivalent to initializing an empty list.
clear : (List t) -> (List t) = @l []

# List concat:
# combines two lists (l1, l2) from left to right.
concat : (List t) -> (List t) -> (List t) = 
  @l1 @l2
  match l1 {
    List/Cons: (List/Cons l1.head (concat l1.tail l2))
    List/Nil: l2
  }

# List add_front:
# adds an element e to the front of list l.
add_front : (List t) -> t -> (List t) = 
  @l @e
  match l {
    List/Cons: (List/Cons e l)
    List/Nil: (List/Cons e List/Nil)
  }

# List append (add_back):
# adds an element e to the back of list l.
append : (List t) -> t -> (List t) = 
  @l @e (concat l (List/Cons e List/Nil))

# list sum:
# returns the sum of all items in the list. 
sum : (List u24) -> u24 = @l
  match l {
    List/Cons: (+ l.head (sum l.tail))
    List/Nil: 0
  }

# List reverse:
# reverses the order of elements in list l.
reverse.aux : (List t) -> (List t) -> (List t) = 
  @acc @l 
  match l {
    List/Nil: acc
    List/Cons: (reverse.aux (List/Cons l.head acc) l.tail)
  }
reverse : (List t) -> (List t) = 
  @l (reverse.aux [] l)

# List length:
# returns the number of elements in list l.
len : (List t) -> u24 = 
  @l 
  match l {
    List/Nil: 0
    List/Cons: (+ 1 (len l.tail))
  }

# List count:
# returns the number of instances of some number n in list l.
count.aux : u24 -> (List u24) -> u24 -> u24 =
  @acc @l @n
  match l {
    List/Nil: acc
    List/Cons: 
      let acc = if (== l.head n) { (+ acc 1) } else { acc }
      (count.aux acc l.tail n)
  }
count = @l @n
  (count.aux 0 l n)

# List index:
# returns the value of a specific list index i, or * if the index doesn't exist.
index : (List t) -> u24 -> (Result t None) = 
  @l @i
  match l {
    List/Cons: 
      switch i {
        0: (Result/Ok l.head)
        _: (index l.tail (i-1))
      }
    List/Nil: (Result/Err *)
  }

# List head:
# returns the first item in the list, or [] if the list is empty.
head : (List t) -> (Result t None) = 
  @l
  match l {
    List/Cons: (Result/Ok l.head)
    List/Nil : (Result/Err *)
  }

# List tail:
# returns the list whithout the first item, or [] if the list is empty.
tail : (List t) -> (List t) = 
  @l
  match l {
    List/Cons: l.tail
    List/Nil: []
  }

# List equals:
# Compares the elements in two lists and returns 1 if they're equal, and 0 otherwise.
# The function cmp compares two values and returns 1 if they're equal, and 0 otherwise. 
equals (xs: (List a)) (ys: (List b)) (cmp: a -> b -> u24) : u24
equals (List/Cons x xs) (List/Cons y ys) cmp = if (cmp x y) { (equals xs ys cmp) } else { 0 }
equals (List/Cons x xs)  List/Nil        cmp = 0
equals  List/Nil        (List/Cons y ys) cmp = 0
equals  List/Nil         List/Nil        cmp = 1

# List pop_front: 
# removes and discards the first item of list l.
# The new list is returned, or [] if the list is empty.
pop_front : (List t) -> (List t) =
  @l
  match l {
    List/Cons: l.tail
    List/Nil: []
  }

# List pop_back:
# removes and discards the the last item of list l.
pop_back : (List t) -> (List t)
pop_back (List/Nil) = List/Nil
pop_back (List/Cons x List/Nil) = List/Nil
pop_back (List/Cons head tail) = (List/Cons head (pop_back tail))
  
# List remove:
# removes the first occurrence of element e from list l.
remove : (List u24) -> u24 -> (List u24) =
  @l @s
  match l {
    List/Cons:
      if (== l.head s) {
        l.tail
      } else {
        (List/Cons l.head (remove l.tail s))
      }
    List/Nil: List/Nil
  }

# List split:
# splits list l into two lists (l1, l2) at index i.
# the second list takes the element at index i during the split.
split : (List t) -> u24 -> ((List t), (List t)) =
  @l @i (split.aux [] l i)

split.aux : (List t) -> (List t) -> u24 -> ((List t), (List t)) =
  @acc @l @i
  match l {
    List/Cons: 
      switch i {
        0: (acc, l)
        _: (split.aux (append acc l.head) l.tail i-1)
      }
    List/Nil: (acc, [])
  }


#################################

def main:
  return head([5, 4, 3, 2, 1])
#  return sum([1, 2, 3])
#  return split([1, 2, 3, 4, 5, 6, 7], 3)
#  return remove([1, 2, 1, 3], 1)
#  return pop_back([1, 2, 3, 4])
#  return pop_front([1, 2, 3])
#  return index([5, 3, 6, 8, 2], 0)
#  return clear([0, 2, 3])
#  return count([1, 2, 3, 3, 3, 4, 4, 5, 3, 1000], 4)
#  return len([1, 2, 3, 4, 4, 4])
#  return reverse([1, 2, 3, 4, 5])
#  return append([1, 2], 3)
#  return add_front([2, 3], 1)
#  return concat([1, 2], [3, 4])
