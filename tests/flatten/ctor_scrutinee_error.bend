# a constructor written as the scrutinee is refused: its fields are
# already in hand, so bind them directly
import Base

def main() -> U32:
  match Con{1, Nil{}}:
    case Con{h, t}:
      h
    case Nil{}:
      0

#|SOME PROOFS FAIL
#|Error:
#|- message  : 'Con{1, Nil{}}' can't be matched (this value is already a constructor: bind its fields directly)
#|Location:
#|5 | def main() -> U32:
#|6>|   match Con{1, Nil{}}:
#|  |   ^^^^^^^^^^^^^^^^^^^^
#|7 |     case Con{h, t}:
#|exit 1
