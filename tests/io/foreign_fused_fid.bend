# The single Bend call must not fuse away an entry also used by FID.
import Base

def hidden(n: U32) -> U32:
  U32.inc(n)

def caller(n: U32) -> U32:
  hidden(n)

def answer() -> IO(U32):
  import "./foreign_fused_fid.c"
  import "./foreign_fused_fid.js"

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print(U32.show(caller(41)))
    value : U32 <- answer()
    IO.print(U32.show(value))

#|42
#|42
