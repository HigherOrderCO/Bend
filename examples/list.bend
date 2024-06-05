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
List/clear = @l
  []

# List concat:
# List l -> List l
# combines two lists (l1, l2) from left to right.
List/concat = @l1 @l2
  match l1 {
    List/Cons: (List/Cons l1.head (List/concat l1.tail l2))
    List/Nil: l2
  }

# List add_front:
# List l -> List l
# adds a non-List element e to the front of list l.
List/add_front = @l @e
  match l {
    List/Cons: (List/Cons e l)
    List/Nil: (List/Cons e List/Nil)
  }

# List append (add_back):
# List l -> List l
# adds a non-list element e to the back of list l.
List/append = @l @e
  (List/concat l (List/Cons e List/Nil))

# list sum:
# List l -> uint
# returns the sum of all items in the list. 
List/sum = @l
  match l {
    List/Cons: (+ l.head (List/sum l.tail))
    List/Nil: 0
  }

# List reverse:
# List l -> List l
# reverses the order of elements in list l.
List/reverse/aux = @acc @l
  match l {
    List/Nil: acc
    List/Cons: (List/reverse/aux (List/Cons l.head acc) l.tail)
  }
List/reverse = @l
  (List/reverse/aux [] l)

# List length:
# List l -> uint
# returns the number of elements in list l.
List/len = @l
  match l {
    List/Nil: 0
    List/Cons: (+ 1 (List/len l.tail))
  }

# List count:
# List l -> uint -> uint
# returns the number of instances of Some s in list l.
List/count/aux = @acc @l @s
  match l {
    List/Nil: acc
    List/Cons: use acc = switch (== l.head s) {
      0: acc;
      _: (+ acc 1);
    }
    (List/count/aux acc l.tail s)
  }
List/count = @l @s
  (List/count/aux 0 l s)

# List index:
# List l -> Some s
# returns the value of a specific list index i, or * if the index doesn't exist.
List/index = @l @i
  match l {
    List/Cons: 
      switch i {
        0: l.head
        _: (List/index l.tail (i-1))
      }
    List/Nil: *
  }

# List head:
# List l -> Some s
# returns the first item in the list, or [] if the list is empty.
List/head = @l
  match l {
    List/Cons: l.head
    List/Nil: []
  }

# List pop_front: 
# List l -> List l
# removes and discards the first item of list l.
# The new list is returned, or [] if the list is empty.
List/pop_front = @l
  match l {
    List/Cons: l.tail
    List/Nil: []
  }

# List pop_back:
# List l -> List l
# removes and discards the the last item of list l.
List/pop_back (List/Nil) = List/Nil
List/pop_back (List/Cons x List/Nil) = List/Nil
List/pop_back (List/Cons head tail) = (List/Cons head (List/pop_back tail))
  
# List remove:
# List l -> Some s -> List l 
# removes the first occurence of element e from list l.
List/remove = @l @s
  match l {
    List/Cons:
      switch (== l.head s) {
        0: (List/Cons l.head (List/remove l.tail s))
        _: l.tail
      }
    List/Nil: List/Nil
  }

# List split:
# list l -> uint i -> (List l, list l)
# splits list l into two lists (l1, l2) at index i.
# the second list takes the element at index i during the split.
List/split/aux = @acc @l @i
  match l {
    List/Cons: 
      switch i {
        0: (acc, l)
        _: (List/split/aux (List/append acc l.head) l.tail i-1)
      }
    List/Nil: *
  }
List/split = @l @i
  (List/split/aux [] l i)

#################################

def main:
  return List/head([5, 4, 3, 2, 1])
#  return List/sum([1, 2, 3])
#  return List/split([1, 2, 3, 4, 5, 6, 7], 3)
#  return List/remove([1, 2, 1, 3], 1)
#  return List/pop_back([1, 2, 3, 4])
#  return List/pop_front([1, 2, 3])
#  return List/index([5, 3, 6, 8, 2], 0)
#  return List/clear([0, 2, 3])
#  return List/count([1, 2, 3, 3, 3, 4, 4, 5, 3, 1000], 4)
#  return List/len([1, 2, 3, 4, 4, 4])
#  return List/reverse([1, 2, 3, 4, 5])
#  return List/append([1, 2], 3)
#  return List/add_front([2, 3], 1)
#  return List/concat([1, 2], [3, 4])