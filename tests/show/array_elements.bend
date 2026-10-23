# a pure main prints an Array of Data by index (Array.write, Array.show)
# whatever its element, and an Array of a Type through a List
import Base

type P is Data:
  P{a: U32, b: Bool}

type W is Type:
  W{
    xs: Array<U32>, ys: Array<P>, zs: Array<F32>, cs: Array<Char>,
    ss: Array<String>, ns: Array<Nat>, as: Array<Array<U32>>
  }

def main() -> W:
  W{
    Array.new(U32, 1n, 5), Array.new(P, 1n, P{3, True{}}),
    Array.new(F32, 1n, 1.5), Array.new(Char, 0n, 'x'),
    Array.new(String, 1n, "h\"i"), Array.new(Nat, 1n, 4n),
    ANode{ALeaf{Array.new(U32, 1n, 3)}, ALeaf{Array.new(U32, 2n, 4)}}
  }

#|W{[5, 5], [P{3, True{}}, P{3, True{}}], [1.5, 1.5], ['x'], ["h\"i", "h\"i"], [4n, 4n], [[3, 3], [4, 4, 4, 4]]}
