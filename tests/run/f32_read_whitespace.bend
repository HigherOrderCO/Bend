# F32.read skips the six ASCII whitespace characters before a number,
# but rejects Unicode whitespace on both the JS and native C lanes.
import Base

def seen(m: Maybe<&2, F32>) -> String:
  match m:
    case None{}:
      "none"
    case Some{v}:
      F32.show(v)

def read(s: String) -> String:
  seen(F32.read(s))

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print(read("1") ++ " "
      ++ read(" 1") ++ " "
      ++ read("\t1") ++ " "
      ++ read("\n1") ++ " "
      ++ read("\u{b}1") ++ " "
      ++ read("\u{c}1") ++ " "
      ++ read("\r1"))
    IO.print(read("\u{a0}1") ++ " "
      ++ read("\u{1680}1") ++ " "
      ++ read("\u{2000}1") ++ " "
      ++ read("\u{2001}1") ++ " "
      ++ read("\u{2002}1") ++ " "
      ++ read("\u{2003}1") ++ " "
      ++ read("\u{2004}1") ++ " "
      ++ read("\u{2005}1") ++ " "
      ++ read("\u{2006}1") ++ " "
      ++ read("\u{2007}1") ++ " "
      ++ read("\u{2008}1") ++ " "
      ++ read("\u{2009}1") ++ " "
      ++ read("\u{200a}1"))
    IO.print(read("\u{2028}1") ++ " "
      ++ read("\u{2029}1") ++ " "
      ++ read("\u{202f}1") ++ " "
      ++ read("\u{205f}1") ++ " "
      ++ read("\u{3000}1") ++ " "
      ++ read("\u{feff}1"))
    IO.print(read(" \t\n\u{b}\u{c}\r-1.25e2") ++ " "
      ++ read("\t+.5") ++ " "
      ++ read("\r+Infinity") ++ " "
      ++ read("\n-NaN"))
    IO.print(read(" \u{a0}1") ++ " "
      ++ read("\u{2003} 1") ++ " "
      ++ read("\u{a0}-1.25e2") ++ " "
      ++ read("\u{feff}inf"))
    IO.print(read("") ++ " "
      ++ read(" \t\n\u{b}\u{c}\r") ++ " "
      ++ read("1 ") ++ " "
      ++ read("1\t") ++ " "
      ++ read("1 2"))

#|1 1 1 1 1 1 1
#|none none none none none none none none none none none none none
#|none none none none none none
#|-125 0.5 inf nan
#|none none none none
#|none none none none none
