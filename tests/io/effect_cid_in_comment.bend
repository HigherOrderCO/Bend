# io (io_effect_cid_in_comment): an effect's C or JS source may say
# CID(...) in a comment. The ids CID(k) and FID(k) are resolved by text
# over the whole source, comments included, so a comment quoting the
# refusal "CID(...) names no constructor or def" failed the build with
# that very line while the effect itself was sound. A comment is now
# left as it is.
import Base

law answer:
  U32 -> IO(U32)

def answer(n):
  import "./effect_cid_in_comment.c"
  import "./effect_cid_in_comment.js"

def main() -> IO(Unit):
  do IO<Unit>:
    a : U32 <- answer(40)
    IO.print(U32.show(a))

#|42
