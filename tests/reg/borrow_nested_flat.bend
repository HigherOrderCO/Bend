# regression witness: a fused reader nested in an argument list, Fin(xs,
# Xs.sum(xs, 0)), runs before Fin receives xs. Before, the pass of xs to
# Fin (an owner) counted the nested read as a later use, so xs was shared
# (a term_keep) and the type went hot. Now a Var argument's later uses are
# the continuation, and the emitter evaluates nested arguments before the
# Var pops. In the C, the program has no term_keep outside the runtime
# and no rfc_seal in WL_CASE(FID_Xs_build_k*).
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

def Fin(xs: Xs, +n: U32) -> U32:
  match xs:
    case X0{}:
      n
    case X1{v, rest}:
      Fin(rest, n)

def F(+xs: Xs) -> U32:
  Fin(xs, Xs.sum(xs, 0))

def main() -> IO(Unit):
  IO.print(U32.show(F(Xs.build(10n, 1))))

#|55
