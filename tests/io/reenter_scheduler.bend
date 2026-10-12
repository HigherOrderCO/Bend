# Nested IO must restore the outer scheduler after both success and failure.
import Base

def Host.nested(mode: U32) -> IO(Unit):
  import "./reenter_scheduler.js"

def main() -> IO(Unit):
  do IO<Unit>:
    Unit <- IO.sleep(1)
    Unit <- Host.nested(0)
    a : Chan(U32) <- IO.fork(U32, IO.pure(U32, 10))
    x : U32 <- IO.join(U32, a)
    Unit <- Host.nested(1)
    b : Chan(U32) <- IO.fork(U32, IO.pure(U32, 20))
    y : U32 <- IO.join(U32, b)
    IO.print(U32.show((x + y : U32)))

#|inner
#|30
