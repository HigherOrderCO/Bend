# regression witness: a reader called where its argument is used later
# (F holds xs past R) and where the call is the argument's last use (main
# passes a fresh list): the held site asks R to lend, and a request wins.
# R lends at both sites; the dying site's continuation holds the list and
# sinks it after the call. Before, the last use made R an owner, so F
# shared the list (a term_keep) and the type went hot. In the C, the
# WL_CASE(FID_F) and WL_CASE(FID_R) segments have no term_keep, and
# WL_CASE(FID_Xs_len) reads through term_peek.
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

def Xs.len(xs: Xs) -> U32:
  match xs:
    case X0{}:
      0
    case X1{v, rest}:
      (Xs.len(rest) + 1 : U32)

def Xs.sum(xs: Xs, +acc: U32) -> U32:
  match xs:
    case X0{}:
      acc
    case X1{v, rest}:
      Xs.sum(rest, (acc + v : U32))

def R(+xs: Xs) -> U32:
  (Xs.len(xs) + Xs.sum(xs, 0) : U32)

def F(+xs: Xs) -> U32:
  (R(xs) + Xs.len(xs) : U32)

def main() -> IO(Unit):
  IO.print(U32.show((F(Xs.build(10n, 1)) + R(Xs.build(5n, 1)) : U32)))

#|95
