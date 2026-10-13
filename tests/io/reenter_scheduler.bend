# Nested IO must restore the outer scheduler after both success and failure.
import Base

def Host.nested(mode: U32) -> IO(Unit):
  import "./reenter_scheduler.js"

def child(text: String) -> IO(Unit):
  IO.print(text)

def main() -> IO(Unit):
  do IO<Unit>:
    Unit <- Host.nested(0)
    Unit <- IO.spawn(Unit, child("child"))
    Unit <- Host.nested(1)
    Unit <- IO.spawn(Unit, child("after-error"))
    IO.print("outer-complete")

#|inner
#|outer-complete
#|child
#|after-error
