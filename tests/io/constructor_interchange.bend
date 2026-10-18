import Base

def Host.constructor_interchange() -> IO(U32):
  import "./constructor_interchange.js"

def main() -> IO(Unit):
  do IO<Unit>:
    status : U32 <- Host.constructor_interchange()
    IO.print(U32.show(status))

#|0
