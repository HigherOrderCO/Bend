# Invalid connect arguments answer EINVAL even when FFI initialization fails.
# The JS twin models a host without bun:ffi; valid Node TCP stays unsupported.
import Base

def ffi_absent() -> IO(Unit):
  import "./tcp_connect_no_ffi.js"

def report(r: Result<&1, &1, U32 & String, Socket>) -> IO(Unit):
  match r:
    case Done{socket}:
      Socket.close(socket)
    case Fail{(code, message)}:
      IO.print(U32.show(code))

def main() -> IO(Unit):
  do IO<Unit>:
    ffi_absent()
    r : Result<&1, &1, U32 & String, Socket> <- TCP.connect("127.0.0.256", 0)
    report(r)

#|22
