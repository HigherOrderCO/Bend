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

#|7
