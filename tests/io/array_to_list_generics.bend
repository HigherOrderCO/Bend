import Base

type Item is Data:
  Item{value: U32}

def digits(xs: List<U32>, acc: U32) -> U32:
  match xs:
    case Nil{}:
      acc
    case Con{head, tail}:
      digits(tail, U32.add(U32.mul(acc, 10), head))

def item_digits(xs: List<Item>, acc: U32) -> U32:
  match xs:
    case Nil{}:
      acc
    case Con{Item{value}, tail}:
      item_digits(tail, U32.add(U32.mul(acc, 10), value))

def numbers(a: Array<U32>) -> List<U32>:
  f = Array.to_list(~U32)
  f(a)

def main() -> Bool:
  Bool.and(
    U32.is_eq(digits(numbers(ANode{ANode{ALeaf{1}, ALeaf{2}},
      ANode{ALeaf{3}, ALeaf{4}}}), 0), 1234),
    U32.is_eq(item_digits(Array.to_list(~Item,
      ANode{ALeaf{Item{5}}, ALeaf{Item{7}}}), 0), 57))

#|True{}
