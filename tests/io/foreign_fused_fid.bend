# The single Bend call must not fuse away an entry also used by FID.
import Base

def imul(a: U32, b: U32) -> U32:
  (a * b : U32)

def caller(a: U32, b: U32) -> U32:
  imul(a, b)

def answer() -> IO(U32):
  import "./foreign_fused_fid.c"
  import "./foreign_fused_fid.js"

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print(U32.show(caller(6, 7)))
    value : U32 <- answer()
    IO.print(U32.show(value))

#|42
#|42
