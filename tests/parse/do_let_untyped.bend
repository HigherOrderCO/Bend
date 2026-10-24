# a do-block let without its type is a let, not the block's result
# followed by a stray "=" (#1787)
import Base

def main() -> IO(Unit):
  do IO<Unit>:
    x = 1
    IO.print(U32.show(x))

#|SOME PROOFS FAIL
#|Error:
#|- expected : a typed bind (x : A <- m) or let (x : A = v)
#|- observed : 'x = 1'
#|Location:
#|6 |   do IO<Unit>:
#|7>|     x = 1
#|  |     ^^^^^
#|8 |     IO.print(U32.show(x))
#|exit 1
