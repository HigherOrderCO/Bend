# A fixed-size absent pattern must not rebuild each input suffix.
import Base

def repeat(n: Nat, +s: String) -> String:
  match n:
    case 0n:
      s
    case 1n+p:
      repeat(p, String.append(s, s))

def main() -> IO(Unit):
  IO.print(Bool.show(String.contains(repeat(21n, "a"), "b")))

#|False
