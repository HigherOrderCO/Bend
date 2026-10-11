# a datatype without constructors goes to BendTT with D.efq : D -> <>,
# which the kernel checks, parameterized or not; a datatype with a live
# field of an empty one is empty through it and gets none
type Void<-A: Type> is Data:

type Emp is Data:

type Bit is Data:
  B0{}
  B1{}

type W is Data:
  W{b: Bit, x: Emp}

law uses:
  for a: Void<Bit>
  for b: Emp
  for c: W
  Bit

def uses(a, b, c):
  B0{}

#|ALL PROOFS CHECK
#|Use --verdict for mathematical validity.
