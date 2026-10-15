# regression witness: a closure captures a binding once, however many
# times its body reads it. A do block's continuations each captured xs
# with the binding's whole count (the reads inside the closure), so every
# capture shared the list (a term_keep per closure segment, the type
# hot) although nothing outside the closure used it again. Now a capture
# is one use: the last one moves the list into the closure. In the C, no
# WL_CASE(FID_main_c*) segment has a term_keep, and the spins of Xs.sum
# read through term_peek.
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

def Xs.sum(xs: Xs, +acc: U32) -> U32:
  match xs:
    case X0{}:
      acc
    case X1{v, rest}:
      Xs.sum(rest, (acc + v : U32))

def main() -> IO(Unit):
  +xs = Xs.build(10n, 1)
  do IO<Unit>:
    Unit <- IO.print(U32.show(Xs.sum(xs, 0)))
    Unit <- IO.print(U32.show((Xs.sum(xs, 0) + Xs.sum(xs, 1) : U32)))
    IO.print(U32.show((Xs.sum(xs, 2) + Xs.sum(xs, 3) + Xs.sum(xs, 4) : U32)))

#|55
#|111
#|174
