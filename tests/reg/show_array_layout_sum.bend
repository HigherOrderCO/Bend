# regression witness: a parameterized sum type is flat as a value and
# boxed per field as an array element; one display layout for both
# printed garbage.
import Base

type D is Data:
  Da{}
  Db{}

type Two<-A: Data, -B: Data> is Data:
  L{a: A, d: D}
  R{b: B, x: U32}

def main() -> (Two<U32, Two<U32, D>> & Array<Two<U32, Two<U32, D>>> & Two<U32, D>):
  (R{L{9, Db{}}, 5}, ANode{ALeaf{R{R{Da{}, 3}, 2}}, ALeaf{L{1, Db{}}}}, R{Db{}, 8})

#|(R{L{9, Db{}}, 5}, [R{R{Da{}, 3}, 2}, L{1, Db{}}], R{Db{}, 8})
