# borrow witness: a borrower passing a field to a storing position. Xs.rev
# stores v, a word: rev~0 stays borrowed (term_peek in its spin) and F has
# no term_keep. Ys.rev stores v, a box field of its rooted parameter: it
# keeps v there (term_peek, then term_keep), its root stays borrowed, and G
# keeps nothing.
import Base

type Xs is Data:
  X0{}
  X1{v: U32, rest: Xs}

def Xs.build(n: Nat, +acc: U32) -> Xs:
  match n:
    case 0n:
      X0{}
    case 1n+p:
      X1{acc, Xs.build(p, (acc + 1 : U32))}

def Xs.sum(xs: Xs, acc: U32) -> U32:
  match xs:
    case X0{}:
      acc
    case X1{v, rest}:
      Xs.sum(rest, (acc + v : U32))

def Xs.cnt(xs: Xs, acc: U32) -> U32:
  match xs:
    case X0{}:
      acc
    case X1{v, rest}:
      Xs.cnt(rest, (acc + 1 : U32))

def Xs.rev(xs: Xs, acc: Xs) -> Xs:
  match xs:
    case X0{}:
      acc
    case X1{v, rest}:
      Xs.rev(rest, X1{v, acc})

type Ys is Data:
  Y0{}
  Y1{v: Xs, rest: Ys}

def Ys.build(n: Nat) -> Ys:
  match n:
    case 0n:
      Y0{}
    case 1n+p:
      Y1{Xs.build(2n, 1), Ys.build(p)}

def Ys.rev(ys: Ys, acc: Ys) -> Ys:
  match ys:
    case Y0{}:
      acc
    case Y1{v, rest}:
      Ys.rev(rest, Y1{v, acc})

def Ys.cnt(ys: Ys, acc: U32) -> U32:
  match ys:
    case Y0{}:
      acc
    case Y1{v, rest}:
      Ys.cnt(rest, (acc + 1 : U32))

def F(+xs: Xs) -> U32:
  r = Xs.rev(xs, X0{})
  (Xs.cnt(xs, 0) + Xs.sum(r, 0) : U32)

def G(+ys: Ys) -> U32:
  r = Ys.rev(ys, Y0{})
  (Ys.cnt(ys, 0) + Ys.cnt(r, 0) : U32)

def main() -> IO(Unit):
  IO.print(U32.show((F(Xs.build(5n, 1)) + G(Ys.build(3n)) : U32)))

#|26
