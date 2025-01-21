def Bar:
  return 0

def Baz:
  return 0

type Bool:
  True
  False
(Foo 0 Bool/False (List/Cons h1 (List/Cons h2 t))) = (Bar h1 h2 t)
(Foo 0 * *) = Baz
(Foo n Bool/False *) = n
(Foo n Bool/True *) = 0


def main() -> _:
  return 0
