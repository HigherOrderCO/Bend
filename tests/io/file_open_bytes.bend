# io (io_file_open_bytes): File.open_bytes opens a path given as bytes,
# with no UTF-8 encoding (#1798). UTF-8 bytes name the file File.open's
# String names; a value past 255 fails with EINVAL and a 0 with EILSEQ. A
# Latin-1 E9 reaches the OS as itself: Linux makes that name, and "caf"
# U+FFFD, all IO.args could give for it, names no file; macOS file systems
# take UTF-8 names only, and refuse it with EILSEQ.
import Base

def ascii(s: String, tail: List<&2, U32>) -> List<&2, U32>:
  match s:
    case SNil{}:
      tail
    case SCon{c, t}:
      Char.to_u32(c) <> ascii(t, tail)

def path(tail: List<&2, U32>) -> List<&2, U32>:
  ascii("/tmp/bend_io_open_bytes_", tail)

def pick(ok: Bool, yes: String, no: String) -> String:
  match ok:
    case True{}:
      yes
    case False{}:
      no

def kept(+got: String, want: String) -> String:
  pick(String.eq(got, want), "E9 kept", got)

# EILSEQ is 92 on macOS and 84 on Linux.
def code(+c: U32) -> String:
  pick(U32.is_eq(c, 92) || U32.is_eq(c, 84), "EILSEQ", U32.show(c))

def shown(r: Result<&1, &1, U32 & String, File>) -> IO(String):
  match r:
    case Done{file}:
      do IO<String>:
        File.close(file)
        return "opened"
    case Fail{(c, message)}:
      IO.pure(String, code(c))

def written(pair: File & Result<&1, &1, U32 & String, Unit>) -> IO(Unit):
  (file, result) = pair
  do IO<Unit>:
    IO.pass(Unit, result)
    File.close(file)

def read(pair: File & Result<&1, &1, U32 & String, String>) -> IO(String):
  (file, result) = pair
  do IO<String>:
    text : String <- IO.pass(String, result)
    File.close(file)
    return text

def latin1(r: Result<&1, &1, U32 & String, File>) -> IO(String):
  match r:
    case Done{file}:
      do IO<String>:
        File.close(file)
        a : Result<&1, &1, U32 & String, File> <- File.open_bytes(path([233]), "r")
        a : String <- shown(a)
        b : Result<&1, &1, U32 & String, File> <-
          File.open("/tmp/bend_io_open_bytes_\u{fffd}", "r")
        b : String <- shown(b)
        return kept(a ++ " " ++ b, "opened 2")
    case Fail{(c, message)}:
      IO.pure(String, kept(code(c), "EILSEQ"))

def main() -> IO(Unit):
  do IO<Unit>:
    f : File <- IO.try(File, File.open_bytes(path([195, 169]), "w"))
    p : File & Result<&1, &1, U32 & String, Unit> <- File.write(f, "hi")
    written(p)
    f : File <- IO.try(File, File.open("/tmp/bend_io_open_bytes_é", "r"))
    p : File & Result<&1, &1, U32 & String, String> <- File.read(f, 16)
    t : String <- read(p)
    IO.print("utf-8 bytes: " ++ t)
    r : Result<&1, &1, U32 & String, File> <- File.open_bytes(path([256]), "r")
    t : String <- shown(r)
    IO.print("past 255: " ++ t)
    r : Result<&1, &1, U32 & String, File> <- File.open_bytes(path([0, 120]), "w")
    t : String <- shown(r)
    IO.print("nul: " ++ t)
    r : Result<&1, &1, U32 & String, File> <- File.open_bytes(path([233]), "w")
    t : String <- latin1(r)
    IO.print("latin-1: " ++ t)

#|utf-8 bytes: hi
#|past 255: 22
#|nul: EILSEQ
#|latin-1: E9 kept
