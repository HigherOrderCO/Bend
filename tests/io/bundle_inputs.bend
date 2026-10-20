# Bundling protects all source identities and restores outputs on write failure.
# JS only: this drives the Bun CLI, as bundle_inline does.
import Base

def Bundle.inputs() -> IO(U32):
  import "./bundle_inputs.js"

def main() -> IO(Unit):
  do IO<Unit>:
    result : U32 <- Bundle.inputs()
    IO.print(U32.show(result))

#|0
