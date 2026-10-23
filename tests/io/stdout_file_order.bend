# io (stdout_file_order): IO.print holds stdout off a terminal, and a
# File.write to /dev/stdout goes straight to the fd, so it writes the
# held text first: the lines come out in program order.
import Base

def done(pair: File & Result<&1, &1, U32 & String, Unit>) -> IO(Unit):
  (file, result) = pair
  File.close(file)

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print("one")
    file : File <- IO.try(File, File.open("/dev/stdout", "a"))
    pair : File & Result<&1, &1, U32 & String, Unit> <-
      File.write(file, "two\n")
    Unit <- done(pair)
    IO.print("three")

#|one
#|two
#|three
