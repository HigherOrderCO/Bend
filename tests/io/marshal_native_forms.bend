# io (io_marshal_native_forms): the native forms cross the effect
# seam canonically, both ways -- a Nat as a number, a Bool as a
# boolean, a Cmp by its tag through the generic ctor lane. The legs
# escalate as io_marshal_char_type does: direct answers, an answer
# inside a Maybe field, a round trip through an argument -- and both
# lanes must print byte-identically, which one value model gives by
# construction.
import Base

law five:
  IO(Nat)

def five():
  import "./marshal_native_forms.js"

law zero:
  IO(Nat)

def zero():
  import "./marshal_native_forms.js"

law wrap:
  IO(Maybe<&2, Nat>)

def wrap():
  import "./marshal_native_forms.js"

law dub:
  for n: Nat
  IO(Nat)

def dub(n):
  import "./marshal_native_forms.js"

law flip:
  for b: Bool
  IO(Bool)

def flip(b):
  import "./marshal_native_forms.js"

law ord:
  IO(Cmp)

def ord():
  import "./marshal_native_forms.js"

law unwrap:
  for m: Maybe<&2, Nat>
  Nat

def unwrap(m):
  match m:
    case None{}:
      0n
    case Some{n}:
      n

law cmp_name:
  for c: Cmp
  String

def cmp_name(c):
  match c:
    case LT{}:
      "lt"
    case EQ{}:
      "eq"
    case GT{}:
      "gt"

law word:
  for b: Bool
  String

def word(b):
  match b:
    case True{}:
      "yes"
    case False{}:
      "no"

law main:
  IO(Unit)

def main():
  do IO<Unit>:
    a  : Nat <- five()
    u1 : Unit <- IO.print(Nat.show(Nat.add(a, 3n)))
    z  : Nat <- zero()
    u2 : Unit <- IO.print(Nat.show(z))
    m  : Maybe<&2, Nat> <- wrap()
    u3 : Unit <- IO.print(Nat.show(unwrap(m)))
    d  : Nat <- dub(7n)
    u4 : Unit <- IO.print(Nat.show(d))
    b  : Bool <- flip(True{})
    u5 : Unit <- IO.print(word(b))
    c  : Cmp <- ord()
    IO.print(cmp_name(c))

#|8
#|0
#|9
#|14
#|no
#|eq
