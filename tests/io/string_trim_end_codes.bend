import Base

def one_char(s: String, c: U32) -> Bool:
  match s:
    case SNil{}:
      False{}
    case SCon{Chr{x}, tail}:
      Bool.and(U32.is_eq(x, c), Nat.is_eq(String.length(tail), 0n))

def keeps_packed(+c: U32) -> Bool:
  one_char(String.trim_end(SCon{Chr{c}, " \t\n\u{b}\u{c}\r"}), c)

def main() -> Bool:
  Bool.and(one_char(String.trim_end(SCon{Chr{65536}, "   "}), 65536),
    Bool.and(keeps_packed(1114144),
      Bool.and(keeps_packed(55305), keeps_packed(4294901773))))

#|True{}
