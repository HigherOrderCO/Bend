# a literal is Base's String, never a file's own: "x" is refused against
# this file's String, whose SCon holds an Empty (#941)
type Empty is Data:

type String is Data:
  SCon{e: Empty, tail: Empty}

def extract(s: String) -> Empty:
  match s:
    case SCon{e, tail}:
      e

def boom() -> Empty:
  extract("x")

#|SOME PROOFS FAIL
#|Error:
#|- expected : a declared constructor (String declares SCon)
#|- observed : "x"
#|Location: boom
#|13 | def boom() -> Empty:
#|14>|   extract("x")
#|   |           ^^^
#|15 | 
#|exit 1
