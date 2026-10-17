import Base

def packed(n: U32) -> String:
  SCon{Char.from_u32(n), ""}

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print(Bool.show(String.ends_with("abc", "")))
    IO.print(Bool.show(String.ends_with("hello", "lo")))
    IO.print(Bool.show(String.ends_with("hello", "ell")))
    IO.print(Bool.show(String.ends_with("a\u{1F600}", "\u{1F600}")))
    IO.print(Bool.show(String.ends_with(packed(1114177), "A")))
    IO.print(Bool.show(String.ends_with("\u{10000}abc", packed(407048291))))
    IO.print(Bool.show(String.ends_with(SCon{Char.from_u32(1114177), "A"}, "A")))
    IO.print(Bool.show(String.ends_with(packed(1114177), packed(1114177))))

#|True
#|True
#|False
#|True
#|False
#|False
#|True
#|True
