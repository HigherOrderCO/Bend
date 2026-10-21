# io (io_args_bytes): IO.args_bytes is IO.args as the OS hands it over,
# with no UTF-8 decoding (#1798): in C a Latin-1 argument "caf" E9 arrives
# as 99 97 102 233, where IO.args gives "caf" U+FFFD, the same String as
# for "caf" E8; the JS lanes get it decoded, as EF BF BD. The harness
# passes no argument, so every lane sees its program, named after this
# test, and no argument.
import Base

def latin1(xs: List<&2, U32>) -> String:
  match xs:
    case Nil{}:
      SNil{}
    case x <> t:
      SCon{Char.from_u32(x), latin1(t)}

def program(xs: List<List<&2, U32>>) -> String:
  match xs:
    case Nil{}:
      "no program"
    case p <> t:
      Bool.show(String.contains(latin1(p), "args_bytes")) ++ " "
        ++ Nat.show(List.length(&1, List<&2, U32>, t))

def main() -> IO(Unit):
  do IO<Unit>:
    xs : List<List<&2, U32>> <- IO.args_bytes()
    IO.print(program(xs))

#|True 0
