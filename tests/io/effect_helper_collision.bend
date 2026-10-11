# An effect named like a helper function of another module's keeps its value.
import Base

def A.run(ms: U32) -> IO(U32):
  import "./effect_helper_worker.js"

def A.run_helper() -> IO(U32):
  import "./effect_helper_name.js"

def main() -> IO(Unit):
  do IO<Unit>:
    y : U32 <- A.run_helper()
    x : U32 <- A.run(1)
    IO.print(U32.show(x) ++ " " ++ U32.show(y))

#|11 22
