# A page bundled into a directory that already holds it (directly, through a
# symlink or a hard link) is refused and left intact; a symlinked page builds
# under its target's name; a missing page keeps Bun's entry error.
import Base

def Bundle.page_input() -> IO(U32):
  import "./bundle_page_input.js"

def main() -> IO(Unit):
  do IO<Unit>:
    value : U32 <- Bundle.page_input()
    IO.print(U32.show(value))

#|0
