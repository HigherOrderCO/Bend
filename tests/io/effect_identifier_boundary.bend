# CID is a macro token, not a suffix of a legal host identifier.
import Base

def answer() -> IO(U32):
  import "./effect_identifier_boundary.c"
  import "./effect_identifier_boundary.js"

def main() -> IO(Unit):
  do IO<Unit>:
    value : U32 <- answer()
    IO.print(U32.show(value))

#|42
