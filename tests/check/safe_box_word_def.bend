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

#|7
