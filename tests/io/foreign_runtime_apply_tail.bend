# With no IO.bind continuation, no Bend call registers Clo~apply. The
# foreign effect must still be able to apply the callback.
import Base

def native.apply(step: U32 -> U32, value: U32) -> IO(U32):
  import "./foreign_runtime_apply.c"
  import "./foreign_runtime_apply.js"

def main() -> IO(U32):
  native.apply(x => (x + 2 : U32), 40)

#|
