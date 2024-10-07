from lib/MyOption import (MyOption, MyOption/bind, MyOption/wrap)

unwrap (val : (MyOption u24)) : u24
unwrap (MyOption/Some x) = x
unwrap (MyOption/None)   = 0

def main() -> MyOption((u24, u24)):
  with MyOption:
    a <- MyOption/Some(1)
    b = unwrap(MyOption/Some(2))
    return wrap((a, b))
