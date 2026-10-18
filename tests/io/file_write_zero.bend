# io (io_file_write_zero): a write the host answers with 0 bytes fails
# with EIO (5) instead of retrying forever (#1445); an empty write still
# succeeds. Stall.open opens /dev/null with every nonempty write stalled;
# it stays open, so its fd is never reused while the stall is on.
import Base

def Stall.open() -> IO(Result<&1, &1, U32 & String, File>):
  import "./file_write_zero.c"
  import "./file_write_zero.js"

def show(pair: File & Result<&1, &1, U32 & String, Unit>) -> IO(File):
  (file, result) = pair
  match result:
    case Done{value}:
      do IO<File>:
        IO.print("ok")
        return file
    case Fail{error}:
      (code, text) = error
      do IO<File>:
        IO.print(U32.show(code))
        return file

def main() -> IO(Unit):
  do IO<Unit>:
    file : File <- IO.try(File, Stall.open())
    pair : File & Result<&1, &1, U32 & String, Unit> <- File.write(file, "abc")
    file : File <- show(pair)
    pair : File & Result<&1, &1, U32 & String, Unit> <- File.write(file, "")
    file : File <- show(pair)
    pair : File & Result<&1, &1, U32 & String, Unit> <-
      File.write_bytes(file, [1, 2])
    file : File <- show(pair)
    return Unit{}

#|5
#|ok
#|5
