# An inline module script in a page is bundled with its .bend imports, and
# an error in it points at the page's own line (#1400).
import Base

def Bundle.inline() -> IO(U32):
  import "./bundle_inline.js"

def main() -> IO(Unit):
  do IO<Unit>:
    value : U32 <- Bundle.inline()
    IO.print(U32.show(value))

#|0
