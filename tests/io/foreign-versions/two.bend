import Base

type Instant is Data:
  Instant{value: U32}

def now() -> IO(Instant):
  import "../foreign_version_two.c"
  import "../foreign_version_two.js"
