# A host zero-byte standard-stream write fails instead of retrying forever.
# JS-only: children inject the host result into the runtime's own io_out.
import Base

def Stream.write_zero() -> IO(U32):
  import "./stream_write_zero.js"

def main() -> IO(Unit):
  do IO<Unit>:
    code : U32 <- Stream.write_zero()
    IO.print(U32.show(code))

#|0
