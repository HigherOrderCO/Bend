# io (io_get_env_eq): a name with a "=" reads no variable. With
# BEND_ENV_EQ set to "x=y", glibc's getenv("BEND_ENV_EQ=x")
# matches that entry and answers "y", so the C and Node lanes read part
# of another variable's value, while Bun failed. Every lane must fail.
import Base

def put_eq() -> IO(Unit):
  import "./get_env_eq.c"
  import "./get_env_eq.js"

law report:
  for tag : String
  for r   : Result<&1, &1, (U32 & String), String>
  IO(Unit)

def report(tag, r):
  match r:
    case Done{value}:
      IO.print(String.append(tag, String.append("=", value)))
    case Fail{error}:
      (code, message) = error
      IO.print(String.append(tag, "=unset"))

law main:
  IO(U32)

def main():
  do IO<U32>:
    p : Unit <- put_eq()
    a : Result<&1, &1, (U32 & String), String> <- IO.get_env("BEND_ENV_EQ")
    x : Unit <- report("exact", a)
    b : Result<&1, &1, (U32 & String), String> <- IO.get_env("BEND_ENV_EQ=x")
    y : Unit <- report("eq", b)
    return 0

#|exact=x=y
#|eq=unset
