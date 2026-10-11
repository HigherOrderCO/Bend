# File.size did its real work through fs.fstatSync, which needs no FFI, and
# then asked io_sys() for the EOVERFLOW errno. io_sys() binds libc through
# bun:ffi, so on a host without it that ask threw where the size had already
# been read, the catch turned it into io_fail(5), and once io_fail stopped
# crashing a good file answered Fail (5, "errno 5") with exit 0. Only a .js
# twin here, so the test pins the JS lane; the C lane, which reads the size
# with stat, never failed. The size is read from a file written first so the
# answer cannot be 0 and pass as something else, and the twin takes the FFI
# table away for exactly one read: see file_size_no_ffi.js.
import Base

def ffi_absent() -> IO(Unit):
  import "./file_size_no_ffi.js"

def ffi_back() -> IO(Unit):
  import "./file_size_no_ffi.js"

def describe(r: Result<&1, &1, U32 & String, U32>) -> String:
  match r:
    case Done{got}:
      "size " ++ U32.show(got)
    case Fail{(code, msg)}:
      "fail " ++ U32.show(code) ++ " " ++ msg

law report:
  for pair: File & Result<&1, &1, U32 & String, U32>
  IO(File)

def report(pair):
  (file, result) = pair
  do IO<File>:
    IO.print(describe(result))
    return file

law wrote:
  for pair: File & Result<&1, &1, U32 & String, Unit>
  IO(File)

def wrote(pair):
  (file, result) = pair
  do IO<File>:
    IO.pass(Unit, result)
    return file

def main() -> IO(Unit):
  do IO<Unit>:
    f : File <- IO.try(File, File.open("/tmp/bend_io_file_size_no_ffi", "w"))
    p : File & Result<&1, &1, U32 & String, Unit> <-
      File.write_bytes(f, [104, 101, 108, 108, 111])
    f : File <- wrote(p)
    u : Unit <- ffi_absent()
    p : File & Result<&1, &1, U32 & String, U32> <- File.size(f)
    v : Unit <- ffi_back()
    f : File <- report(p)
    File.close(f)

#|size 5
