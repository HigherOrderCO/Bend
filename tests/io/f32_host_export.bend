# Scalar F32 inputs round before Bend uses them, including curried module
# arguments and a foreign answer; the host's binary64 precision must not
# survive into F32 addition.
import Base

def F32.host_export() -> IO(U32):
  import "./f32_host_export.js"

def F32.host_input() -> IO(F32):
  import "./f32_host_export.js"

def main() -> IO(Unit):
  do IO<Unit>:
    status : U32 <- F32.host_export()
    u : Unit <- IO.print(U32.show(status))
    value : F32 <- F32.host_input()
    IO.print(F32.show(F32.add(value, 1.0)))

#|0
#|16777216
