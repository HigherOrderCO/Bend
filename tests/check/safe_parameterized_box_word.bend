# a literal is Base's U32, never a file's own: 7 is refused against a
# Box whose constructor is named U32
type Bit is Data:
  False{}
  True{}

type Word is Data:
  WNil{}
  WCon{head: Bit, tail: Word}

type Box<T: Type> is Data:
  U32{d: Word}

def bit() -> Box<Bit>:
  7

def word() -> Box<Word>:
  7

def main() -> Box<Bit>:
  bit()

#|SOME PROOFS FAIL
#|Error:
#|- expected : a declared constructor (Box declares U32)
#|- observed : 7
#|Location: bit
#|14 | def bit() -> Box<Bit>:
#|15>|   7
#|   |   ^
#|16 | 
#|exit 1
