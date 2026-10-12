# A pure __proto__ def is an own, enumerable JS module export, without
# changing the export object's prototype.
import Base

def Module.export_keys() -> IO(U32):
  import "./module_export_keys.js"

def main() -> IO(Unit):
  do IO<Unit>:
    value : U32 <- Module.export_keys()
    IO.print(U32.show(value))

#|0
