# a match whose arms are constant ops builds a table only from ops whose JS
# the compiler can run (it holds just fround and imul): sqrt and floor rows
# make a plain match, as before, not a compile error
import Base

def table(n: U32) -> F32:
  match n:
    case 0:
      F32.sqrt(4.0)
    case 1:
      F32.floor(2.5)
    case _:
      F32.add(1.0, 2.0)

law main:
  IO(Unit)

def main():
  IO.print(F32.show(F32.add(table(0), F32.add(table(1), table(7)))))

#|7
