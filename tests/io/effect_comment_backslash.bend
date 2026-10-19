# io (io_effect_comment_backslash): a backslash-newline continues a C comment
# (CRLF, trailing spaces and a split CID too) but not a JS one, so a CID after
# it is an id in JS and comment text in C.
import Base

def answer() -> IO(U32):
  import "./effect_comment_backslash.c"
  import "./effect_comment_backslash.js"

def main() -> IO(Unit):
  do IO<Unit>:
    a : U32 <- answer()
    IO.print(U32.show(a))

#|42
