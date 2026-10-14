# io (io_marshal_string_scalar): a String is the effect's JS string,
# walked by Char in every lane. The boundary scalars (55295, 57344,
# 1114111) cross as one-Char strings, a Char answer's code reads back
# exact, and a host's lone surrogate comes in as U+FFFD: torn ("a" then
# D800) counts 2.
import Base

law low:
  IO(String)

def low():
  import "./marshal_string_scalar.js"

law high:
  IO(String)

def high():
  import "./marshal_string_scalar.js"

law apex:
  IO(String)

def apex():
  import "./marshal_string_scalar.js"

law edge:
  IO(Char)

def edge():
  import "./marshal_string_scalar.js"

law torn:
  IO(String)

def torn():
  import "./marshal_string_scalar.js"

law count:
  for s: String
  U32

def count(s):
  match s:
    case SNil{}:
      0
    case SCon{h, t}:
      U32.add(1, count(t))

law code_of:
  for c: Char
  U32

def code_of(c):
  match c:
    case Chr{n}:
      n

law main:
  IO(U32)

def main():
  do IO<U32>:
    a : String <- low()
    b : String <- high()
    c : String <- apex()
    d : Char <- edge()
    e : String <- torn()
    u : Unit <- IO.print(U32.show(count(a)))
    v : Unit <- IO.print(U32.show(count(b)))
    w : Unit <- IO.print(U32.show(count(c)))
    x : Unit <- IO.print(U32.show(code_of(d)))
    y : Unit <- IO.print(U32.show(count(e)))
    return 0

#|1
#|1
#|1
#|55295
#|2
