import Base

def main() -> IO(Unit):
  IO.print(Nat.show(String.length("A😀" ++ Char.show(Char.from_u32(4294967295)) ++ "λ𐐀")))

#|5
