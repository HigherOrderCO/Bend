# Capture-aware arm sharing: a Nat literal's expansion duplicates the
# default arm's body at every bound, so one source body becomes a spin over
# its captured a and b. The spin must pass those captures by position; the
# two calls below exercise both a literal hit and the default.
import Base

def deep(n: Nat, a: U32, b: U32) -> U32:
  match n:
    case 300n:
      U32.sub(a, b)
    case _:
      U32.sub(a, b)

def main() -> U32:
  U32.add(deep(300n, 10, 3), deep(7n, 10, 3))

#|14
