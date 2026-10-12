import Base

def Host.exit_status() -> IO(U32):
  import "./exit_status.js"

def main() -> IO(Unit):
  do IO<Unit>:
    status : U32 <- Host.exit_status()
    IO.print(U32.show(status))

#|0
