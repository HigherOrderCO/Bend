# An imported effect and a dotted root effect must register independently.
import Base
import ./effect_namespace_lib.bend as L

def effect_namespace_lib.value() -> IO(U32):
  import "./effect_namespace_collision.js"

def main() -> IO(Unit):
  do IO<Unit>:
    imported : U32 <- L.value()
    root : U32 <- effect_namespace_lib.value()
    Unit <- IO.print(U32.show(imported))
    IO.print(U32.show(root))

#|1
#|2
