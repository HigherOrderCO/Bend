# Search at Char boundaries, including codes outside Unicode scalars.
import Base

def one(x: U32) -> String:
  SCon{Char.from_u32(x), SNil{}}

def self_codes(xs: List<&2, U32>) -> Bool:
  match xs:
    case Nil{}:
      True{}
    case +x <> t:
      Bool.and(String.contains(one(x), one(x)), self_codes(t))

def apply(f: @+p: String -> Bool, p: String) -> Bool:
  f(p)

def unfold(-h: Char, -t: String, -p: String) ->
  {String.contains(SCon{h, t}, p) ==
    String.contains.if(t, p, String.starts_with(SCon{h, t}, p)) : Bool}:
  {==}

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print(Bool.show(String.contains("", "")))
    IO.print(Bool.show(String.contains("", "a")))
    IO.print(Bool.show(String.contains("abc", "")))
    IO.print(Bool.show(String.contains("abc", "abc")))
    IO.print(Bool.show(String.contains("abc", "bc")))
    IO.print(Bool.show(String.contains("abc", "abcd")))
    IO.print(Bool.show(String.contains("abc", "d")))
    IO.print(Bool.show(String.contains("aaaab", "aaab")))
    IO.print(Bool.show(String.contains("a😀b😀", "😀b")))
    IO.print(Bool.show(String.contains("a😀b😀", "😁")))
    IO.print(Bool.show(String.contains("a𐀀b", "b")))
    IO.print(Bool.show(apply(String.contains("a😀b"), "😀")))
    IO.print(Bool.show(String.contains(one(0), one(0))))
    IO.print(Bool.show(String.contains(one(55296), one(0))))
    IO.print(Bool.show(String.contains(one(1114209), "a")))
    IO.print(Bool.show(String.contains(String.append(one(1114209), "b"), "ab")))
    IO.print(Bool.show(String.contains(one(4294967295), one(2047))))
    IO.print(Bool.show(String.contains(String.append(one(55296), one(56320)), one(65536))))
    IO.print(Bool.show(String.contains(String.append(one(1114209), "ab"), "ab")))
    IO.print(Bool.show(String.contains(String.append("a", one(4294967295)), one(4294967295))))
    IO.print(Bool.show(String.contains(one(4294967295), one(4294967294))))
    IO.print(Bool.show(self_codes([0, 65, 55295, 55296, 56320, 57343, 57344,
      65536, 1114111, 1114112, 1114209, 4194304, 2147483648, 4294967295])))

#|True
#|False
#|True
#|True
#|True
#|False
#|False
#|True
#|True
#|False
#|True
#|True
#|True
#|False
#|False
#|False
#|False
#|False
#|True
#|True
#|False
#|True
