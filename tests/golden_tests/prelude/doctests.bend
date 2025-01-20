(Foo 0 false (List/Cons h1 (List/Cons h2 t))) = (A h1 h2 t)  # Most specific
(Foo 0 * *) = B                                             # General 0 case
(Foo n false *) = n                                          # General false case
(Foo n true *) = 0  

def A():
  return 0

def B():
  return 0

def main():
  return 0
