from lib/MyOption import (MyOption, MyOption/bind, MyOption/wrap)

def main() -> MyOption((u24, u24)):
  with MyOption:
    a <- MyOption/Some(1)
    b <- MyOption/Some(2)
    return wrap((a, b))
