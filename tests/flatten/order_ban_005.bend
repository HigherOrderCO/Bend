# THE ORDER BAN, joint (#1134): in one match, the column on mv decides
# first and closes t, so t's column is an order error
import Base

type Ty is Data:
  TStr{}
  TU32{}

def f(t: Ty, mv: Maybe<&2, U32>) -> String:
  match mv t:
    case None{} _:
      "none"
    case Some{v} TStr{}:
      "str"
    case Some{v} TU32{}:
      "u32"

def main() -> IO(Unit):
  IO.print(f(TStr{}, Some{1}))

#|SOME PROOFS FAIL
#|Error:
#|- message  : 't' can't be matched in this position (it is matched after a local statement or after a match on a later binder, it was already matched, or it is a def)
#|Location:
#| 9 | def f(t: Ty, mv: Maybe<&2, U32>) -> String:
#|10>|   match mv t:
#|   |            ^
#|11 |     case None{} _:
#|exit 1
