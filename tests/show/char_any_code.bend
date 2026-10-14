# show (show_char_any_code): a Char holds any u32 code, the same in every
# lane (#1080). A code that is no Unicode scalar (a surrogate, past
# U+10FFFF) shows as \u{hex} and reads back as itself; D800 and DC00
# built apart stay two Chars after an append, not one U+10000.
import Base

def one(n: U32) -> String:
  SCon{Char.from_u32(n), SNil{}}

def code(s: String) -> U32:
  match s:
    case SNil{}:
      0
    case SCon{h, t}:
      Char.to_u32(h)

def text() -> String:
  String.append(one(55296), String.append(one(56320),
    String.append(one(1114112), one(4294967295))))

law main:
  Pair(String, Pair(Nat, Pair(U32, Char)))

def main():
  (text(), (String.length(text()), (code(one(4294967295)), Char.from_u32(57343))))

#|("\u{d800}\u{dc00}\u{110000}\u{ffffffff}", 4n, 4294967295, '\u{dfff}')
