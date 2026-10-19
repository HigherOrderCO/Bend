# A checked book's namespace survives JS string escaping in its module key.
import Base

def Module.quoted_namespace() -> IO(U32):
  import "./module_quoted_namespace.js"

def main() -> IO(Unit):
  do IO<Unit>:
    value : U32 <- Module.quoted_namespace()
    IO.print(U32.show(value))

#|0
