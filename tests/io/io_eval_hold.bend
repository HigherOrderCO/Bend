# io (io_eval_hold): while another thread holds the pool, as an IO.eval
# does, the IO loop applies a continuation that calls a bang's def. The
# loop must evaluate it without the pool or the device: sequentially,
# answering through its own root. Hold.on returns once the other thread
# holds the lock, so the continuation always runs in that state. Then an
# IO.eval runs the bang's def on the eval thread. sum(d, 0) adds the
# leaves 0 .. 2^d - 1: 32640 for d = 8, 2147450880 for d = 16.
import Base

def Hold.on() -> IO(Unit):
  import "./io_eval_hold.c"
  import "./io_eval_hold.js"

def Hold.off() -> IO(Unit):
  import "./io_eval_hold.c"
  import "./io_eval_hold.js"

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
    Unit <- Hold.on()
    z : U32 <- IO.pure(U32, 0)
    Unit <- IO.print(U32.show(U32.add(z, sum!(8n, 0))))
    Unit <- Hold.off()
    t : Chan(U32) <- IO.fork(U32, IO.eval(U32, u => sum!(16n, 0)))
    s : U32 <- IO.join(U32, t)
    IO.print(U32.show(s))

#|32640
#|2147450880
