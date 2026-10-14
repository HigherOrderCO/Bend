# io (io_effect_undefined_word): an effect's C source may say the word
# undefined, in a comment or a string. The C emitter once refused any
# output holding the word, as a JS undefined leaked into the C it
# generates, and read the effect sources spliced into it too, so the
# twin's comment failed the native build while the other lanes ran. A
# leaked undefined is an undeclared name, which clang refuses itself.
import Base

law word.len:
  U32 -> IO(U32)

def word.len(n):
  import "./effect_undefined_word.c"
  import "./effect_undefined_word.js"

def main() -> IO(Unit):
  do IO<Unit>:
    a : U32 <- word.len(2)
    IO.print(U32.show(a))

#|18
