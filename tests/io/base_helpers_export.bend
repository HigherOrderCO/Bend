import Base

def Nat.base_helpers_export() -> IO(U32):
  import "./base_helpers_export.js"

def main() -> IO(Unit):
  do IO<Unit>:
    value : U32 <- Nat.base_helpers_export()
    IO.print(U32.show(value))

#|0
