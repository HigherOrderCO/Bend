import Base

def classify(op: IO.OP<U32>) -> U32:
  match op:
    case Emit{v}: v
    case _: 91

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print(U32.show(classify(Exit{7})))
    IO.exit(23)
    IO.print("unreachable")

#|91
#|exit 23
