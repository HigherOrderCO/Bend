# An entry may spell a sibling's canonical namespace, but cannot make that
# sibling's foreign source resolve Base's None or Con to those declarations.
import Base
import ./cid_capture_lib.bend as L

type Forgery is Data:
  cid_capture_lib.None{}
  cid_capture_lib.Con{x: U32, y: U32, z: U32}

def keep(f: Forgery) -> U32:
  match f:
    case cid_capture_lib.None{}:
      0
    case cid_capture_lib.Con{x, y, z}:
      (x + y + z : U32)

def show(m: Maybe<U32>) -> String:
  match m:
    case None{}:
      "None"
    case Some{value}:
      "Some " ++ U32.show(value)

def main() -> IO(Unit):
  do IO<Unit>:
    m : Maybe<U32> <- L.peek()
    IO.print(show(m))
    xs : List<U32> <- L.list()
    IO.print(U32.show(keep(cid_capture_lib.Con{1, 2, 3})))

#|None
#|6
