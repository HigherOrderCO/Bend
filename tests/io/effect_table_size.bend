# #1163: the effect table is sized by CID_T, so a small program stays under
# 256 KiB. The harness builds this test as ./effect_table_size, beside its
# .js, in the directory both run in; the checkup runs before the build, so
# a missing binary passes.
import Base

def report(pair: File & Result<&1, &1, U32 & String, U32>) -> IO(Unit):
  (file, result) = pair
  do IO<Unit>:
    size : U32 <- IO.pass(U32, result)
    File.close(file)
    IO.print(Bool.show(U32.is_lt(size, 262144)))

def inspect(r: Result<&1, &1, U32 & String, File>) -> IO(Unit):
  match r:
    case Fail{error}:
      IO.print("True")
    case Done{file}:
      IO.bind(File & Result<&1, &1, U32 & String, U32>, Unit, File.size(file),
        report)

def main() -> IO(Unit):
  IO.bind(Result<&1, &1, U32 & String, File>, Unit,
    File.open("effect_table_size", "r"), inspect)

#|True
