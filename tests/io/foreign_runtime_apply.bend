# A foreign effect applies a Bend closure through Clo~apply and returns
# its result to Bend, where IO.print writes it.
import Base

def native.apply(step: U32 -> U32, value: U32) -> IO(U32):
  import "./foreign_runtime_apply.c"
  import "./foreign_runtime_apply.js"

def main() -> IO(Unit):
  do IO<Unit>:
    result : U32 <- native.apply(x => (x + 2 : U32), 40)
    IO.print(U32.show(result))

#|42
