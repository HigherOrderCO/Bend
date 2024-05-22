##############################
# Author: Ematth, 2024
##############################
### Singly-Linked List Type Definition: ###

# The List type is builtin, so we don't need to declare it.
# But this is how it's defined in the builtins file.
# type List:
#   Cons { head, ~tail }
#   Nil

###########################################

# List clear:
# List l -> list l
# clears all elements from list l.
# (this might be completely useless...)
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
List/len/aux = @acc @l
  match l {
    List/Nil: acc
    List/Cons: (List/len/aux (+ acc 1) l.tail)
  }
List/len = @l
  (List/len/aux 0 l)

# List count:
# List l -> Some s -> uint
# returns the number of instances of element s in list l.
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
        _: (List/index l.tail (- i 1))
      }
    List/Nil: *
  }

# List pop_front: 
# List l -> (List l, Some s)
# removes the first item from the front of list l, or [] if the list is empty.
# Both the popped element and the new list are returned.
List/pop_front = @l
  match l {
    List/Cons: (l.head, l.tail) 
    List/Nil: []
  }

# List pop_back:
# List l -> List l
# removes the first item from the back of list l.
List/pop_back/aux = @acc @l
  match l {
    List/Nil: List/Nil
    List/Cons: 
      use x = switch acc {
        0: *
        _: l.head
      }
      use y = switch acc {
        0: List/Nil
        1: List/Nil
        _: l.tail
      }
      (List/Cons x (List/pop_back/aux (- acc 1) y))
  }
List/pop_back = @l
  switch (- (List/len l) 1) {
    0: []
    _:  (List/pop_back/aux (- (List/len l) 1) l)
  }
  
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
        _: (List/split/aux (List/append acc l.head) l.tail (- i 1))
      }
    List/Nil: *
  }
List/split = @l @i
  (List/split/aux [] l i)

#################################

#main = (List/split [1, 2, 3, 4, 5, 6, 7] 3)
#main = (List/remove [1, 2, 1, 3] 1)
#main = (List/pop_back [])
#main = (List/pop_front [1, 2, 3])
#main = (List/index [1, 2, 3, 4, 5] 4)
#main = (List/clear [0, 2, 3])
#main = (List/count [1, 2, 3, 3, 3, 4, 4, 5, 3, 1000] 3)
#main = (List/len [1, 2, 3, 4, 4, 4])
#main = (List/reverse [1, 2, 3, 4, 5])
#main = (List/append [1, 2] 3)
#main = (List/add_front [2, 3] 1)
#main = (List/concat [1, 2] [3, 4])