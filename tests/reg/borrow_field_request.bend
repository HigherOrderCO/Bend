# regression witness: View opens its +record and passes a field to Sum,
# whose other site (Probe) passes a dying field. Before, that last use
# made Sum an owner, an owned use of a field made View own the record,
# and Loop, which uses the record after View, shared it (a term_keep per
# box word). Now Probe's site asks Sum to own and View's asks it to lend
# (the record is View's borrow): Sum lends, Probe's continuation sinks
# the field, View borrows, and Loop shares nothing. In the C,
# WL_CASE(FID_Loop) and WL_CASE(FID_View) have no term_keep.
import Base

type Xs is Data:
  X0{}
  X1{v: U32, rest: Xs}

type Live is Data:
  Live{xs: Xs, n: U32}

def Xs.build(n: Nat, +acc: U32) -> Xs:
  match n:
    case 0n:
      X0{}
    case 1n+p:
      X1{acc, Xs.build(p, (acc + 1 : U32))}

def Sum(xs: Xs) -> U32:
  match xs:
    case X0{}:
      0
    case X1{v, rest}:
      (Sum(rest) + v : U32)

def View(+s: Live) -> U32:
  Live{xs, n} = s
  (Sum(xs) + n : U32)

def Step(s: Live) -> Live:
  Live{xs, n} = s
  Live{xs, (n + 1 : U32)}

def Probe(s: Live) -> U32:
  Live{xs, n} = s
  (Sum(xs) * 2 : U32)

def Loop(k: Nat, +s: Live) -> IO(Unit):
  match k:
    case 0n:
      IO.print(U32.show(Probe(s)))
    case 1n+p:
      do IO<Unit>:
        Unit <- IO.print(U32.show(View(s)))
        Loop(p, Step(s))

def main() -> IO(Unit):
  Loop(3n, Live{Xs.build(4n, 1), 0})

#|10
#|11
#|12
#|20
