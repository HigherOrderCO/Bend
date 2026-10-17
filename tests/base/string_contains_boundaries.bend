import Base

def packed(n: U32) -> String:
  SCon{Char.from_u32(n), ""}

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print(Bool.show(String.contains("abc", "")))
    IO.print(Bool.show(String.contains("hello", "ell")))
    IO.print(Bool.show(String.contains("hello", "x")))
    IO.print(Bool.show(String.contains("a\u{1F600}b", "\u{1F600}")))
    IO.print(Bool.show(String.contains(packed(1114177), "A")))
    IO.print(Bool.show(String.contains("\u{10000}abc", packed(407048291))))
    IO.print(Bool.show(String.contains(SCon{Char.from_u32(1114177), "A"}, "A")))

#|True
#|True
#|False
#|True
#|False
#|False
#|True
