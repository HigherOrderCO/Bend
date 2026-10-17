import Base

law empty_prefix:
  for s: String
  {String.starts_with(s, "") == True{} : Bool}

def empty_prefix(s):
  {==}

law head_prefix:
  for t: String
  {String.starts_with(SCon{'a', t}, "a") == True{} : Bool}

def head_prefix(t):
  {==}

law mismatch:
  for t: String
  for p: String
  {String.starts_with(SCon{'a', t}, SCon{'b', p}) == False{} : Bool}

def mismatch(t, p):
  {==}

law two_heads:
  for t: String
  {String.starts_with(SCon{'a', SCon{'b', t}}, "ab") == True{} : Bool}

def two_heads(t):
  {==}

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print(Bool.show(String.starts_with("abc", "")))

#|True
