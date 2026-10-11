# THE ORDER BAN, reported (#1134): t is bound before mv, and the match on
# mv closes t, so the nested match on t dies at the ban, and the message
# names binder order as a cause, not only a def or a consumed binder
import Base

type Ty is Data:
  TStr{}
  TU32{}

def f(t: Ty, mv: Maybe<&2, U32>) -> String:
  match mv:
    case None{}:
      "none"
    case Some{v}:
      match t:
        case TStr{}:
          "str"
        case TU32{}:
          "u32"

def main() -> IO(Unit):
  IO.print(f(TStr{}, Some{1}))

#|SOME PROOFS FAIL
#|Error:
#|- message  : 't' can't be matched in this position (it is matched after a local statement or after a match on a later binder, it was already matched, or it is a def)
#|Location:
#|14 |     case Some{v}:
#|15>|       match t:
#|   |             ^
#|16 |         case TStr{}:
#|exit 1
