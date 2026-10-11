# io (io_request_wildcard): an IO.OP match's own default arm takes only
# what an IO.OP is (Emit, Halt), never a request. decode's case _ takes a
# purely built Halt and prints "other"; intercept's case _ meets the
# request IO.print("SECRET") applies to its continuation, and every lane
# refuses it instead of running the user's arm on it (which printed
# "wild" in C, and after $FFI went, in JS too). See
# request_out_of_band for the same refusal without a default arm.
import Base

law intercept.at:
  for op: IO.OP<U32>
  IO(U32)

def intercept.at(op):
  match op:
    case Emit{v}:
      do IO<U32>:
        u : Unit <- IO.print("emitted")
        return v
    case _:
      do IO<U32>:
        u : Unit <- IO.print("wild")
        return 9

law intercept:
  IO(U32)

def intercept():
  intercept.at(IO.print("SECRET")(U32, x => Emit{7}))

law decode:
  for op: IO.OP<U32>
  String

def decode(op):
  match op:
    case Emit{v}: U32.show(v)
    case _: "other"

law main:
  IO(U32)

def main():
  s = decode(Halt{1, "bye"})
  do IO<U32>:
    u : Unit <- IO.print(s)
    r : U32 <- intercept()
    return r

#|other
#|bend: runtime fail-stop
#|exit 1
