# Packed U32 Chars may contain ASCII payload units; only Char boundaries count.
import Base

def wide() -> String:
  SCon{Chr{4212768},
    SCon{Chr{65},
      SCon{Chr{4327521},
        SCon{Chr{4294967295},
          SCon{Chr{55296},
            SCon{Chr{32},
              SCon{Chr{65536},
                SCon{Chr{97}, SNil{}}
              }
            }
          }
        }
      }
    }
  }

def codes(s: String) -> String:
  match s:
    case SNil{}:
      ""
    case SCon{Chr{h}, t}:
      U32.show(h) ++ ":" ++ codes(t)

def chars(xs: List<&2, Char>) -> String:
  match xs:
    case Nil{}:
      ""
    case Con{Chr{h}, t}:
      U32.show(h) ++ ":" ++ chars(t)

def parts(xs: List<&2, String>) -> String:
  match xs:
    case Nil{}:
      ""
    case Con{h, t}:
      "[" ++ codes(h) ++ "]" ++ parts(t)

def read_result(m: Maybe<&2, U32>) -> String:
  match m:
    case None{}:
      "None"
    case Some{n}:
      U32.show(n)

def apply_text(f: String -> String, s: String) -> String:
  f(s)

def check(+s: String) -> IO(Unit):
  +pieces = String.split(s, ' ')
  do IO<Unit>:
    IO.print(codes(String.to_upper(s)))
    IO.print(codes(String.to_lower(s)))
    IO.print(codes(String.reverse(s)))
    IO.print(codes(String.take(s, 3n)))
    IO.print(codes(String.drop(s, 3n)))
    IO.print(codes(String.trim_start(" \t\r\n" ++ s)))
    IO.print(chars(String.to_list(s)))
    IO.print(parts(pieces))
    IO.print(codes(String.join(pieces, SCon{Chr{55296}, SNil{}})))
    IO.print(codes(String.concat(pieces)))
    IO.print(read_result(U32.read(SCon{Chr{4294706}, SNil{}})))
    IO.print(apply_text(String.to_upper, "aß"))

def main() -> IO(Unit):
  check(wide())

#|4212768:65:4327521:4294967295:55296:32:65536:65:
#|4212768:97:4327521:4294967295:55296:32:65536:97:
#|97:65536:32:55296:4294967295:4327521:65:4212768:
#|4212768:65:4327521:
#|4294967295:55296:32:65536:97:
#|4212768:65:4327521:4294967295:55296:32:65536:97:
#|4212768:65:4327521:4294967295:55296:32:65536:97:
#|[4212768:65:4327521:4294967295:55296:][65536:97:]
#|4212768:65:4327521:4294967295:55296:55296:65536:97:
#|4212768:65:4327521:4294967295:55296:65536:97:
#|None
#|Aß
