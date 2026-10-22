# HTML character references and ASCII whitespace determine which scripts run.
import Base

def HTML.script_types() -> IO(U32):
  import "./html_script_types.js"

def main() -> IO(Unit):
  do IO<Unit>:
    result : U32 <- HTML.script_types()
    IO.print(U32.show(result))

#|0
