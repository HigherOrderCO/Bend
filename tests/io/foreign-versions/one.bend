import Base

type Instant is Data:
  Instant{value: U32}

def now() -> IO(Instant):
  import "../foreign_version_one.c"
  import "../foreign_version_one.js"
