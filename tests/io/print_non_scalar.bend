# io (io_print_non_scalar): a Char that is no Unicode scalar (a
# surrogate, past U+10FFFF) keeps its code inside Bend (#1080), and is
# written as U+FFFD (EF BF BD) where text leaves it, in every lane:
# before, C wrote its raw bytes (ED A0 80) and JS died at its Chr.
import Base

law txt:
  String

def txt():
  SCon{Chr{97}, SCon{Chr{55296}, SCon{Chr{56320}, SCon{Chr{1114112},
    SCon{Chr{98}, SNil{}}}}}}

law main:
  IO(Unit)

def main():
  do IO<Unit>:
    u : Unit <- IO.print(txt())
    v : Unit <- IO.print(U32.show(Char.to_u32(Chr{55296})))
    IO.print_err(txt())

#|a���b
#|55296
#|a���b
