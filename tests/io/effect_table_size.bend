import Base

def report(pair: File & Result<&1, &1, U32 & String, U32>) -> IO(Unit):
  (file, result) = pair
  do IO<Unit>:
    size : U32 <- IO.pass(U32, result)
    File.close(file)
    IO.print(Bool.show(U32.is_lt(size, 262144)))

def inspect(args: List<String>) -> IO(Unit):
  match args:
    case Nil{}:
      IO.die(Unit, 1, "missing program path")
    case path <> rest:
      do IO<Unit>:
        file : File <- IO.try(File, File.open(path, "r"))
        pair : File & Result<&1, &1, U32 & String, U32> <- File.size(file)
        report(pair)

def main() -> IO(Unit):
  do IO<Unit>:
    args : List<String> <- IO.args()
    inspect(args)

#|True
