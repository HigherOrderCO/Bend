# A host closure reaches a Bend tail call as it is: one with a j of its own,
# one with a Bend closure's fields copied on, and every one once
# Function.prototype has a j, run their own code, not that j.
import Base

def Clo.host_tail() -> IO(U32):
  import "./js_closure_host.js"

def main() -> IO(Unit):
  do IO<Unit>:
    value : U32 <- Clo.host_tail()
    IO.print(U32.show(value))

#|0
