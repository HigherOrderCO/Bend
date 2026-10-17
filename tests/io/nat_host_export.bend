# Exported Nat arguments reject invalid whole numbers, including curried calls.
# Nat.show also preserves the constructor overflow reached by its Base body.
import Base

def Nat.host_export() -> IO(U32):
  import "./nat_host_export.js"

def main() -> IO(Unit):
  do IO<Unit>:
    value : U32 <- Nat.host_export()
    IO.print(U32.show(value))

#|0
