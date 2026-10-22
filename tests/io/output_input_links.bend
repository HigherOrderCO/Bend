# Output files must not overwrite inputs through a path, symlink or hard link.
import Base

def Output.input_links() -> IO(U32):
  import "./output_input_links.js"

def main() -> IO(Unit):
  do IO<Unit>:
    result : U32 <- Output.input_links()
    IO.print(U32.show(result))

#|0
