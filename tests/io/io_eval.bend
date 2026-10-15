# io (io_eval): IO.eval computes a parallel sum off the IO loop thread
# while the loop runs main's own sum; join waits for the eval's answer.
# sum(d, 0) adds the leaves 0 .. 2^d - 1: 523776 for d = 10 and
# 2147450880 for d = 16.
import Base

def sum(+d: Nat, +i: U32) -> U32:
  match d:
    case 0n:
      i
    case 1n+p:
      +k = U32.mul(i, 2)
      l r = sum(p, k) sum(p, U32.add(k, 1))
      U32.add(l, r)

def main() -> IO(Unit):
  do IO<Unit>:
    t : Chan(U32) <- IO.fork(U32, IO.eval(U32, u => sum(16n, 0)))
    Unit <- IO.print(U32.show(sum(10n, 0)))
    s : U32 <- IO.join(U32, t)
    IO.print(U32.show(s))

#|523776
#|2147450880
