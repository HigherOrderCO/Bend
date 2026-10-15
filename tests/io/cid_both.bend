# a bare CID(Tick) where the importing file has both a constructor and a
# foreign def named Tick names neither: CID(#Tick) and CID(:Tick) pick one
import Base

type T is Data:
  Tick{}
  Tock{}

def Tick() -> IO(U32):
  import "./cid_both.js"

def main() -> IO(Unit):
  do IO<Unit>:
    n : U32 <- Tick()
    IO.print(U32.show(n))

#|Error: CID(Tick) names both a constructor and a def
#|exit 1
