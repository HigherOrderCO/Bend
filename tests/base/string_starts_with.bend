# A matched prefix needs no information about the remaining haystack.
import Base

def empty_prefix(-s: String) ->
  {String.starts_with(s, "") == True{} : Bool}:
  {==}

def one_char_prefix(-t: String) ->
  {String.starts_with(SCon{'a', t}, "a") == True{} : Bool}:
  {==}

def known_prefix(-t: String) ->
  {String.starts_with(SCon{'a', SCon{'b', t}}, "ab") == True{} : Bool}:
  {==}

def head_mismatch(-t: String, -p: String) ->
  {String.starts_with(SCon{'a', t}, SCon{'b', p}) == False{} : Bool}:
  {==}

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print(Bool.show(String.starts_with("", "")))
    IO.print(Bool.show(String.starts_with("abc", "")))
    IO.print(Bool.show(String.starts_with("", "a")))
    IO.print(Bool.show(String.starts_with("abc", "a")))
    IO.print(Bool.show(String.starts_with("abc", "abc")))
    IO.print(Bool.show(String.starts_with("abc", "abcd")))
    IO.print(Bool.show(String.starts_with("abc", "b")))
    IO.print(Bool.show(String.starts_with("abc", "abd")))
    IO.print(Bool.show(String.starts_with("😀abc", "😀a")))
    IO.print(Bool.show(String.starts_with("😀abc", "😁")))

#|True
#|True
#|False
#|True
#|True
#|False
#|False
#|False
#|True
#|False
