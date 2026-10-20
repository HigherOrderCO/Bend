# an empty match on U32 is not total. bend3's hint asked for the number
# match's default case; bend's U32 is a plain wrapper datatype, so the
# same \{} asks for its one constructor instead
import Base

law main:
  @n:U32 -> U32

def main():
  \{}

#|SOME PROOFS FAIL
#|Error:
#|- message  : this match doesn't cover U32
#|Location: main
#| 9 | def main():
#|10>|   \{}
#|   |   ^^^
#|11 | 
#|Note: add a case for `U32` (its fields may be dependent, so a case with them may not check), or a catch-all case.
#|exit 1
