#This test aims to see if every builtin Map function behaves correctly and returns the expected result

type Bool:
  True
  False 

# Returns values from a map
def test1() -> (u24, u24):
  m = {1: 1, 2: 2, 3: 3}
  return (m[1], m[2])

# Takes two lists and uses one as keys and the other as values, returning a map
def test2(m: Map(T), xs: List(u24), ys: List(T)) -> Map(T):
  match xs:
    case List/Nil:
      return Map/Leaf
    case List/Cons:
      match ys:
        case List/Nil:
          return Map/Leaf
        case List/Cons:
          return test2(Map/set(m, xs.head, ys.head), xs.tail, ys.tail)

# Sets a value if the given node is empty, otherwise returns the map
def test3(m: Map(T), x: u24, v: T) -> Map(T):
  (map, val) = Map/get_check(m, x)
  match val:
    case Maybe/Some:
      return m
    case Maybe/None:
      return Map/set(m, x, v)

# Checks if a map contains a given key, and if it does, returns the assigned to the key, otherwise it returns the given value
def test4(m: Map(T), x: u24, v: T) -> T:
  match Map/contains(m, x):
    case 1:
      (value, map) = Map/get(m, x)
      return value
    case 0:
      return v

#Checks if a generic map contains a given key, and if it does, applies a function to the value, otherwise it returns the map
def test5(m: Map(T), key: u24) -> Map(T):
  def self (x: T) -> T:
    return x
  match Map/contains(m, key):
    case 1:
      return Map/map(m, key, self())
    case 0:
      return m

#Checks if a u24 map contains a given key, and if it does, applies a function to the value, otherwise it returns the map
def test6(m: Map(u24), key: u24) -> Map(u24):
  def addtwo (x: u24) -> u24:
    return (x + 2)
  match Map/contains(m, key):
    case 1:
      return Map/map(m, key, addtwo())
    case 0:
      return m


def main():
  m1 = {0: 42, 1: 7, 2: 13, 3: 255, 4: 100}  # all u24 numbers
  m2 = {0: Bool/True, 1: Bool/False, 2: Bool/True, 3: Bool/False, 4: Bool/True}  # booleans
  m3 = {0: Nat/Zero, 
        1: Nat/Succ(Nat/Zero), 
        2: Nat/Succ(Nat/Succ(Nat/Zero)), 
        3: Nat/Succ(Nat/Succ(Nat/Succ(Nat/Zero)))}  # natural numbers
  m4 = {0: 42, 
        1: Bool/True, 
        2: Nat/Succ(Nat/Zero), 
        3: 255, 
        4: Bool/False, 
        5: Nat/Zero}  # mixed types

  m5 = {7: 100, 
        3: 42, 
        11: 255, 
        2: 13, 
        5: 77, 
        9: 144}  # unordered keys with u24 values

  m6 = {0: 1, 
        5: 25, 
        2: 9, 
        8: 64, 
        1: 4, 
        3: 16, 
        7: 49, 
        4: 36}  # square numbers as values
  
  return (test1(), test2(Map/Leaf, List/Cons(1, List/Nil), List/Cons(2, List/Nil)), test3(m1, 2, 42), test4(m2, 1, Bool/True), test5(m3, 0), test6(m4, 4), test5(m5, 7), test6(m6, 8))
