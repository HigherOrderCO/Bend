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
# List l -> list l
# clears all elements from list l. This is equivalent to initializing an empty list.
clear = @l []

# List concat:
# List l -> List l
# combines two lists (l1, l2) from left to right.
concat = @l1 @l2
  match l1 {
    List/Cons: (List/Cons l1.head (concat l1.tail l2))
    List/Nil: l2
  }

# List add_front:
# List l -> List l
# adds a non-List element e to the front of list l.
add_front = @l @e
  match l {
    List/Cons: (List/Cons e l)
    List/Nil: (List/Cons e List/Nil)
  }

# List append (add_back):
# List l -> List l
# adds a non-list element e to the back of list l.
append = @l @e
  (concat l (List/Cons e List/Nil))

# list sum:
# List l -> uint
# returns the sum of all items in the list. 
sum = @l
  match l {
    List/Cons: (+ l.head (sum l.tail))
    List/Nil: 0
  }

# List reverse:
# List l -> List l
# reverses the order of elements in list l.
reverse.aux = @acc @l
  match l {
    List/Nil: acc
    List/Cons: (reverse.aux (List/Cons l.head acc) l.tail)
  }
reverse = @l
  (reverse.aux [] l)

# List length:
# List l -> uint
# returns the number of elements in list l.
len = @l
  match l {
    List/Nil: 0
    List/Cons: (+ 1 (len l.tail))
  }

# List count:
# List l -> uint -> uint
# returns the number of instances of Some s in list l.
count.aux = @acc @l @s
  match l {
    List/Nil: acc
    List/Cons: use acc = switch (== l.head s) {
      0: acc;
      _: (+ acc 1);
    }
    (count.aux acc l.tail s)
  }
count = @l @s
  (count.aux 0 l s)

# List index:
# List l -> Some s
# returns the value of a specific list index i, or * if the index doesn't exist.
index = @l @i
  match l {
    List/Cons: 
      switch i {
        0: l.head
        _: (index l.tail (i-1))
      }
    List/Nil: *
  }

# List head:
# List l -> Some s
# returns the first item in the list, or [] if the list is empty.
head = @l
  match l {
    List/Cons: l.head
    List/Nil: []
  }

# List tail:
# List l -> List
# returns the list except for the first item in it, or [] if the list is empty.
def tail(l):
  match l:
    case List/Cons:
      return l.tail
    case List/Nil:
      return []

# List equals:
# List xs, List ys, function cmp -> 1|0
# Compares the elements in two lists and returns 1 if they're equal, and 0 otherwise.
# The function cmp compares two values and returns 1 if they're equal, and 0 otherwise. 
equals xs ys cmp = match xs {
  List/Cons: match ys {
    List/Cons: if (cmp xs.head ys.head) {
      (equals xs.tail ys.tail cmp)
    } else {
      0
    }
    List/Nil: 0
  }
  List/Nil: match ys {
    List/Cons: 0
    List/Nil: 1
  }
}

# List pop_front: 
# List l -> List l
# removes and discards the first item of list l.
# The new list is returned, or [] if the list is empty.
pop_front = @l
  match l {
    List/Cons: l.tail
    List/Nil: []
  }

# List pop_back:
# List l -> List l
# removes and discards the the last item of list l.
pop_back (List/Nil) = List/Nil
pop_back (List/Cons x List/Nil) = List/Nil
pop_back (List/Cons head tail) = (List/Cons head (pop_back tail))
  
# List remove:
# List l -> Some s -> List l 
# removes the first occurrence of element e from list l.
remove = @l @s
  match l {
    List/Cons:
      switch (== l.head s) {
        0: (List/Cons l.head (remove l.tail s))
        _: l.tail
      }
    List/Nil: List/Nil
  }

# List split:
# list l -> uint i -> (List l, list l)
# splits list l into two lists (l1, l2) at index i.
# the second list takes the element at index i during the split.
split = @l @i (split.aux [] l i)

split.aux = @acc @l @i
  match l {
    List/Cons: 
      switch i {
        0: (acc, l)
        _: (split.aux (append acc l.head) l.tail i-1)
      }
    List/Nil: *
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
