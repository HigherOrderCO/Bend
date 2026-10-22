# JS exports keep own __proto__ keys and distinguish imported dep:value
# from a root dep.value, including when the export object is spread.
import Base

def Module.export_keys() -> IO(U32):
  import "./module_export_keys.js"

def main() -> IO(Unit):
  do IO<Unit>:
    value : U32 <- Module.export_keys()
    IO.print(U32.show(value))

#|0
