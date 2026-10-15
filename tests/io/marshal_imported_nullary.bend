# io (io_marshal_imported_nullary): the imported module's constructors
# cross the effect seam by their own names, in both lanes -- `{$: "Off"}`
# in JS as GUIDE.md says, CID(Off) in C. io_far_types covers the case
# where every constructor carries a field; the nullary one is this.
import Base
import ./marshal_imported_nullary_type.bend as M

law main:
  IO(Unit)

def main():
  do IO<Unit>:
    a : M.Tag <- M.tag.off()
    u : Unit <- IO.print(M.tag.name(a))
    b : M.Tag <- M.tag.on()
    IO.print(M.tag.name(b))

#|off
#|on
