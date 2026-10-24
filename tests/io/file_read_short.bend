# io (io_file_read_short): File.read fills its buffer instead of answering
# the first read(2). A FIFO writer sends sixteen bytes one at a time; a
# read of sixteen must answer sixteen, not the first one.
import Base

law short.file:
  U32 -> IO(File)

def short.file(n):
  import "./file_read_short.c"
  import "./file_read_short.js"

law show:
  for m: File & Result<&1, &1, (U32 & String), String>
  IO(Unit)

def show(m):
  (f, r) = m
  do IO<Unit>:
    +s : String <- IO.pass(String, r)
    n : U32 <- IO.pure(U32, U32.from_nat(String.length(s)))
    a : Unit <- IO.print(U32.show(n))
    b : Unit <- IO.print(s)
    File.close(f)

law main:
  IO(Unit)

def main():
  do IO<Unit>:
    f : File <- short.file(16)
    m : File & Result<&1, &1, (U32 & String), String> <- File.read(f, 16)
    show(m)

#|16
#|xxxxxxxxxxxxxxxx
