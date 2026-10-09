# String.order walks both strings once and hands nothing back: the
# order of two equal 50000-char strings is one tail call per char (a
# non-tail walk overflows the JS lane's stack near 50000), and the
# order is decided by the first differing char, or by length when one
# string is the other's prefix; String.eq, is_lt, is_le, is_gt and
# is_ge read the same walk
import Base

def rep(n: Nat, +c: Char, acc: String) -> String:
  match n:
    case 0n:
      acc
    case 1n+p:
      rep(p, c, SCon{c, acc})

def show(c: Cmp) -> String:
  match c:
    case LT{}:
      "LT"
    case EQ{}:
      "EQ"
    case GT{}:
      "GT"

def ord(a: String, b: String) -> String:
  show(String.order(a, b))

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print(ord("", ""))
    IO.print(ord("", "a"))
    IO.print(ord("a", ""))
    IO.print(ord("abc", "abc"))
    IO.print(ord("abc", "abd"))
    IO.print(ord("abd", "abc"))
    IO.print(ord("ab", "abc"))
    IO.print(ord("abc", "ab"))
    IO.print(ord("B", "a"))
    IO.print(ord("é", "z"))
    IO.print(Bool.show(String.eq("abc", "abc")))
    IO.print(Bool.show(String.eq("abc", "abcd")))
    IO.print(Bool.show(String.is_lt("abc", "abd")))
    IO.print(Bool.show(String.is_le("abc", "abc")))
    IO.print(Bool.show(String.is_gt("abd", "abc")))
    IO.print(Bool.show(String.is_ge("abc", "abd")))
    IO.print(ord(rep(50000n, Chr{97}, ""), rep(50000n, Chr{97}, "")))
    IO.print(ord(rep(50000n, Chr{97}, "b"), rep(50000n, Chr{97}, "c")))
    IO.print(ord(rep(50000n, Chr{97}, "b"), rep(50000n, Chr{97}, "")))
#|EQ
#|LT
#|GT
#|EQ
#|LT
#|GT
#|LT
#|GT
#|LT
#|GT
#|True
#|False
#|True
#|True
#|True
#|False
#|EQ
#|LT
#|GT
