# The JS reader must reject a long integer with an invalid suffix without
# splitting its digits between two overlapping repetitions. Build it with
# a tail-recursive accumulator so string construction does not exhaust the stack.
import Base

def digits(n: Nat, acc: String) -> String:
  match n:
    case 0n:
      acc
    case 1n+p:
      digits(p, SCon{'1', acc})

def seen(m: Maybe<&2, F32>) -> String:
  match m:
    case None{}:
      "none"
    case Some{v}:
      F32.show(v)

def main() -> IO(Unit):
  IO.print(seen(F32.read(digits(100000n, "x"))))

#|none
