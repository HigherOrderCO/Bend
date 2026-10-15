# a literal is Base's U32, never a file's own: 7 is refused against a
# Box whose constructor is named U32
type Bit is Data:
  True{}
  False{}

type Word is Data:
  WNil{}
  WCon{h: Bit, t: Word}

type Box is Data:
  U32{d: Word}

def U32() -> Bit:
  True{}

def main() -> Box:
  7

#|SOME PROOFS FAIL
#|Error:
#|- expected : a declared constructor (Box declares U32)
#|- observed : 7
#|Location: main
#|17 | def main() -> Box:
#|18>|   7
#|   |   ^
#|19 | 
#|exit 1
