# io (io_effect_comment_backslash): a JS `//` comment ends at the newline
# even after a trailing backslash, so the CID on the next line is still an id
# to replace. The scanner once applied C's backslash-newline continuation to
# JS comments too, so `// ...\` swallowed the following `io_eff(CID(...))`
# line, leaving CID unresolved and the effect dead at runtime
# (`ReferenceError: CID is not defined`). C keeps its continuation: a C comment
# splices a backslash-newline, so its quoted CID stays inside the comment.
import Base

def answer() -> IO(U32):
  import "./effect_comment_backslash.js"

def main() -> IO(Unit):
  do IO<Unit>:
    a : U32 <- answer()
    IO.print(U32.show(a))

#|42
