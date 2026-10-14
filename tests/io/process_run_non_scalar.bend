# io (io_process_run_non_scalar): a String that crosses into a child
# process, as argv inside a List or as stdin, writes a non-scalar Char
# as U+FFFD like IO.print does (#1080); before, JS passed its inner
# code units raw, so the child saw U+FFFD plus junk.
import Base

law txt:
  String

def txt():
  SCon{Chr{97}, SCon{Chr{55296}, SCon{Chr{1114112}, SCon{Chr{98},
    SNil{}}}}}

def out(r: Result<&1, &1, U32 & String, U32 & (String & String)>) ->
  String:
  match r:
    case Done{(code, (stdout, stderr))}:
      stdout
    case Fail{error}:
      "unexpected failure"

def main() -> IO(Unit):
  do IO<Unit>:
    a : Result<&1, &1, U32 & String, U32 & (String & String)> <-
      Process.run("/bin/cat", [], txt(), 128, 1000)
    x : Unit <- IO.print("stdin: " ++ out(a))
    b : Result<&1, &1, U32 & String, U32 & (String & String)> <-
      Process.run("/bin/echo", ["x", txt()], "", 128, 1000)
    IO.print("argv: " ++ out(b))

#|stdin: a��b
#|argv: x a��b
#|
