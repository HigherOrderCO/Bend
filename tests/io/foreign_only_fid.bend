# A C effect calls a function that has no Bend callsite.
import Base

def imul(a: U32, b: U32) -> U32:
  (a * b : U32)

def answer() -> IO(U32):
  import "./foreign_only_fid.c"
  import "./foreign_only_fid.js"

def main() -> IO(Unit):
  do IO<Unit>:
    value : U32 <- answer()
    IO.print(U32.show(value))

#|42
