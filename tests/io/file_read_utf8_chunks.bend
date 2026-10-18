# Each read decodes independently: neither partial UTF-8 nor BOM state leaks.
import Base

def file_path(r: Result<&1, &1, U32 & String, String>) -> String:
  match r:
    case Done{dir}: dir ++ "/bend_io_utf8_chunks"
    case Fail{e}: "/tmp/bend_io_utf8_chunks"

def wrote(m: File & Result<&1, &1, U32 & String, Unit>) -> IO(Unit):
  (f, r) = m
  do IO<Unit>:
    IO.pass(Unit, r)
    File.close(f)

def check(name: String, ok: Bool) -> IO(Unit):
  match ok:
    case True{}: IO.print(name)
    case False{}: IO.die(Unit, 1, "FAIL " ++ name)

def checked(m: File & Result<&1, &1, U32 & String, String>, want: String, name: String) -> IO(File):
  (f, r) = m
  do IO<File>:
    text : String <- IO.pass(String, r)
    check(name, String.eq(text, want))
    return f

def reads(+path: String) -> IO(Unit):
  do IO<Unit>:
    f : File <- IO.try(File, File.open(path, "w"))
    m : File & Result<&1, &1, U32 & String, Unit> <- File.write_bytes(f, [226, 130, 172, 239, 187, 191, 239, 187, 191])
    wrote(m)
    f : File <- IO.try(File, File.open(path, "r"))
    m : File & Result<&1, &1, U32 & String, String> <- File.read(f, 2)
    f : File <- checked(m, SCon{Chr{65533}, SNil{}}, "partial prefix")
    m : File & Result<&1, &1, U32 & String, String> <- File.read(f, 1)
    f : File <- checked(m, SCon{Chr{65533}, SNil{}}, "independent continuation")
    m : File & Result<&1, &1, U32 & String, String> <- File.read(f, 3)
    f : File <- checked(m, SCon{Chr{65279}, SNil{}}, "first BOM preserved")
    m : File & Result<&1, &1, U32 & String, String> <- File.read(f, 3)
    f : File <- checked(m, SCon{Chr{65279}, SNil{}}, "next BOM preserved")
    File.close(f)

def main() -> IO(Unit):
  do IO<Unit>:
    r : Result<&1, &1, U32 & String, String> <- IO.get_env("TMPDIR")
    reads(file_path(r))

#|partial prefix
#|independent continuation
#|first BOM preserved
#|next BOM preserved
