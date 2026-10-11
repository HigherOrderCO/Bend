import Base

def peek() -> IO(Maybe<U32>):
  import "./cid_capture_lib.js"
  import "./cid_capture_lib.c"

def list() -> IO(List<U32>):
  import "./cid_capture_lib.js"
  import "./cid_capture_lib.c"

#|SOME PROOFS FAIL
#|Error: 2 defs rely on unsafe or foreign code:
#|- peek
#|- list
#|exit 1
