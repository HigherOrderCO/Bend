# A runnable IO chain must let host microtasks progress without parking.
import Base

def Host.start() -> IO(Unit):
  import "./host_microtask_progress.js"

def Host.seen() -> IO(U32):
  import "./host_microtask_progress.js"

def busy(fuel: Nat) -> IO(Unit):
  match fuel:
    case 0n:
      IO.pure(Unit, Unit{})
    case 1n+p:
      IO.bind(U32, Unit, Host.seen(), seen => busy(p))

def main() -> IO(Unit):
  do IO<Unit>:
    Unit <- Host.start()
    Unit <- busy(2000n)
    answer : U32 <- Host.seen()
    IO.print(U32.show(answer))

#|1
