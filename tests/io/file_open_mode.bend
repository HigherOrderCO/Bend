# io (io_file_open_mode): File.open speaks "r", "w", "a" and "x" in
# every backend; any other mode answers Fail(EINVAL). The JS kit
# once took fs.openSync's wider vocabulary, so "r+" and "w+" opened
# where C refused them; Node's own "wx" stays refused. "x" creates a
# new path and refuses one that exists, leaving its bytes alone. Bend
# has no unlink, so the "x" path is random.
import Base

law answer:
  for r: Result<&1, &1, (U32 & String), File>
  IO(Unit)

def answer(r):
  match r:
    case Done{value}:
      do IO<Unit>:
        u : Unit <- File.close(value)
        IO.print("opened")
    case Fail{error}:
      (code, message) = error
      IO.print(message)

law probe:
  for mode: String
  IO(Unit)

def probe(mode):
  do IO<Unit>:
    r : Result<&1, &1, (U32 & String), File> <- File.open("/tmp/bend_io_mode.txt", mode)
    answer(r)

law wrote:
  for w: File & Result<&1, &1, (U32 & String), Unit>
  IO(Unit)

def wrote(w):
  (f, r) = w
  do IO<Unit>:
    u : Unit <- IO.pass(Unit, r)
    File.close(f)

law keep:
  for r: Result<&1, &1, (U32 & String), File>
  IO(Unit)

def keep(r):
  match r:
    case Done{value}:
      do IO<Unit>:
        w : File & Result<&1, &1, (U32 & String), Unit> <- File.write(value, "keep")
        u : Unit <- wrote(w)
        IO.print("created")
    case Fail{error}:
      (code, message) = error
      IO.print(message)

law show:
  for m: File & Result<&1, &1, (U32 & String), String>
  IO(Unit)

def show(m):
  (f, r) = m
  do IO<Unit>:
    data : String <- IO.pass(String, r)
    p    : Unit <- IO.print(data)
    File.close(f)

def at(n: U32) -> String:
  String.append("/tmp/bend_io_excl_", U32.show(n))

def excl(+n: U32) -> IO(Unit):
  do IO<Unit>:
    a : Result<&1, &1, (U32 & String), File> <- File.open(at(n), "x")
    b : Unit <- keep(a)
    c : Result<&1, &1, (U32 & String), File> <- File.open(at(n), "x")
    d : Unit <- answer(c)
    f : File <- IO.try(File, File.open(at(n), "r"))
    m : File & Result<&1, &1, (U32 & String), String> <- File.read(f, 64)
    show(m)

law main:
  IO(U32)

def main():
  do IO<U32>:
    a : Unit <- probe("w")
    b : Unit <- probe("r")
    c : Unit <- probe("r+")
    d : Unit <- probe("w+")
    e : Unit <- probe("wx")
    r : Result<&1, &1, (U32 & String), U32> <- IO.random_u32()
    n : U32 <- IO.pass(U32, r)
    x : Unit <- excl(n)
    return 0

#|opened
#|opened
#|Invalid argument
#|Invalid argument
#|Invalid argument
#|created
#|File exists
#|keep
