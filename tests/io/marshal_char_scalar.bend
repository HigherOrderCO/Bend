# io (io_marshal_char_scalar): a Char holds any u32 code (#1080), and an
# effect gets Bend's own text: echo hands Chr{55296} back as 55296. Only
# the effect's str_out, as text leaves Bend, turns a code that is no
# Unicode scalar (a surrogate, past U+10FFFF) into U+FFFD: seen hands it
# back as 65533. The boundary scalars (55295, 57344, 1114111) cross as
# they are.
import Base

law echo:
  for c: Char
  IO(Char)

def echo(c):
  import "./marshal_char_scalar.js"

law seen:
  for c: Char
  IO(Char)

def seen(c):
  import "./marshal_char_scalar.js"

law main:
  IO(U32)

def main():
  do IO<U32>:
    a : Char <- echo(Chr{55295})
    b : Char <- echo(Chr{57344})
    c : Char <- echo(Chr{1114111})
    d : Char <- echo(Chr{55296})
    u : Unit <- IO.print(U32.show(Char.to_u32(a)))
    v : Unit <- IO.print(U32.show(Char.to_u32(b)))
    w : Unit <- IO.print(U32.show(Char.to_u32(c)))
    x : Unit <- IO.print(U32.show(Char.to_u32(d)))
    e : Char <- seen(Chr{55296})
    y : Unit <- IO.print(U32.show(Char.to_u32(e)))
    return 0

#|55295
#|57344
#|1114111
#|55296
#|65533
