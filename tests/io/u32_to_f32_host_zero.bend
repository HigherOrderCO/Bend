# A host's -0 is unsigned zero as a U32, but retains its sign as an F32.
import Base

def uint_zero() -> IO(U32):
  import "./u32_to_f32_host_zero.c"
  import "./u32_to_f32_host_zero.js"

def float_zero() -> IO(F32):
  import "./u32_to_f32_host_zero.c"
  import "./u32_to_f32_host_zero.js"

def main() -> IO(Unit):
  do IO<Unit>:
    u : U32 <- uint_zero()
    IO.print(U32.show(F32.bits(U32.to_f32(u))))
    f : F32 <- float_zero()
    IO.print(U32.show(F32.bits(f)))

#|0
#|2147483648
