# regression witness: an Array element type that applies a type variable
# (F(U32) under a generic F) is open as well -- its layout depends on F,
# so emission refuses it like a bare Array<T>.
import Base

law gcount:
  for -F: Type -> Type
  for  a: Array<F(U32)>
  U32

def gcount(F, a):
  match a:
    case ALeaf{x}:
      d = x
      1
    case ANode{xs, ys}:
      U32.add(gcount(F, xs), gcount(F, ys))

def Id(A: Type) -> Type:
  A

law main.out:
  U32

def main.out():
  gcount(Id, ANode{ALeaf{3}, ALeaf{4}})

law main:
  IO(Unit)

def main():
  IO.print(U32.show(main.out()))

#|Error: an open Array element type
#|exit 1
