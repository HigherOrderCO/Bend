# A constructor and foreign request keep distinct identities. The foreign
# header selects :Tick explicitly because bare Tick would be ambiguous.
import Base

type T is Data:
  Tick{}
  Tock{}

def Tick() -> IO(U32):
  import "./effect_ctr_name.js"

def main() -> IO(Unit):
  do IO<Unit>:
    n : U32 <- Tick()
    IO.print(U32.show(n))

#|7
