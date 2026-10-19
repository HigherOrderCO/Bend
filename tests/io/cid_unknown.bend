# a CID header selector names a constructor or a def: a misspelled one
# fails the build (it once built a tag no match knew, and the program took
# the other branch)
import Base

type Box is Data:
  Full{v: U32}
  Hole{}

def mk() -> IO(Box):
  import "./cid_unknown.js"

def show(b: Box) -> String:
  match b:
    case Full{v}:
      "full"
    case Hole{}:
      "hole"

def main() -> IO(Unit):
  do IO<Unit>:
    b : Box <- mk()
    IO.print(show(b))

#|Error: CID(#Ful) names no public constructor or def
#|exit 1
