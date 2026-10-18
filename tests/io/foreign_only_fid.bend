# A C effect calls a function that has no Bend callsite.
import Base

def hidden(n: U32) -> U32:
  U32.inc(n)

def answer() -> IO(U32):
  import "./foreign_only_fid.c"

def main() -> IO(Unit):
  do IO<Unit>:
    value : U32 <- answer()
    IO.print(U32.show(value))

#|42
