# A foreign def whose source registers no effect is refused by name when
# its request runs.
import Base

def unregistered() -> IO(U32):
  import "./effect_unregistered.js"

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print("before")
    n : U32 <- unregistered()
    IO.print(U32.show(n))

#|before
#|bend: no effect registers unregistered
#|exit 1
