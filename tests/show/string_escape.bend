# show (show_string_escape): a string always prints as a string and a
# char as a char. A code point the literal cannot show as itself (a
# control, 127, a surrogate, one past U+10FFFF) prints as \u{hex}, and
# the parser reads \u{hex} back, so every printed value parses again;
# the empty string is "". Before, such a value fell back to its
# constructors (SCon{Chr{U32{...}}, ...}, SNil{}) because the language
# had no escape for it. Pinned on the interpreter, C and JS: the
# compiled printer spells the same escapes; char_any_code pins a
# surrogate and a code past U+10FFFF.
import Base

law main:
  Pair(String, Pair(String, Pair(Char, Pair(String, Pair(Char, String)))))

def main():
  ("a\u{1}b\u{7f}\t\"q\"", ("", ('\u{1b}', ("\u{1F600}", ('\u{7}', "it's")))))

#|("a\u{1}b\u{7f}\t\"q\"", "", '\u{1b}', "😀", '\u{7}', "it's")
