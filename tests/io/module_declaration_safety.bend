# Both generated module files must be checked before either is written.
import Base

def Module.declaration_safety() -> IO(U32):
  import "./module_declaration_safety.js"

def main() -> IO(Unit):
  do IO<Unit>:
    result : U32 <- Module.declaration_safety()
    IO.print(U32.show(result))

#|0
