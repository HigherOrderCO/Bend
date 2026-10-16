import Base

def probe() -> IO(Maybe<&1, U32>):
  import "./effect_comment_newlines.c"
  import "./effect_comment_newlines.js"

def result(m: Maybe<&1, U32>) -> IO(Unit):
  match m:
    case Some{n}:
      IO.print(U32.show(n))
    case None{}:
      IO.print("wrong result")

def main() -> IO(Unit):
  IO.bind(Maybe<&1, U32>, Unit, probe(), result)

#|42
