# A partial match is legal while an Empty binder remains live. The foreign
# keep prevents dead-code pruning from hiding its Efq tail from either
# emitter. Neither sink constructs Empty or invokes the retained closure.
import Base

law absurd.arm:
  for e: Empty
  for b: Bool
  U32

def absurd.arm(e, b):
  match b:
    case True{}: 1

law keep:
  for f: Empty -> Bool -> U32
  IO(U32)

def keep(f):
  import "./match_absurd_tail.c"
  import "./match_absurd_tail.js"

law main:
  IO(Unit)

def main():
  do IO<Unit>:
    n : U32 <- keep(absurd.arm)
    Unit <- IO.print(U32.show(n))
    IO.print("ok")

#|1
#|ok
