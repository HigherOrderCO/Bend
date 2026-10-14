# io (io_get_env_nul): a name with an embedded NUL reads no
# variable. The C lane rejects it before asking the host, but Node's
# environment proxy truncated the key at the NUL, so "HOME\0suffix"
# answered Done for HOME instead of Fail. Every lane must fail.
import Base

law report:
  for tag : String
  for r   : Result<&1, &1, (U32 & String), String>
  IO(Unit)

def report(tag, r):
  match r:
    case Done{value}:
      IO.print(String.append(tag, "=set"))
    case Fail{error}:
      (code, message) = error
      IO.print(String.append(tag, "=unset"))

law main:
  IO(U32)

def main():
  do IO<U32>:
    a : Result<&1, &1, (U32 & String), String> <- IO.get_env("HOME")
    x : Unit <- report("HOME", a)
    b : Result<&1, &1, (U32 & String), String> <- IO.get_env("HOME\0suffix")
    y : Unit <- report("nul", b)
    return 0

#|HOME=set
#|nul=unset
