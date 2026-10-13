# Keys that fold to one word in a JS name stay apart, as does a def named
# like a runtime function, and each keeps its closures to itself.
import Base

type Box is Type:
  H{f: U32 -> U32}

def use(box: Box, x: U32) -> U32:
  match box:
    case H{f}: f(x)

def foo.bar(x: U32) -> U32:
  use(H{y => U32.add(U32.mul(y, 10), 1)}, x)

def foo_bar(x: U32) -> U32:
  use(H{y => U32.add(U32.mul(y, 10), 2)}, x)

def Foo.bar(x: U32) -> U32:
  U32.add(U32.mul(x, 10), 3)

def FOO_BAR(x: U32) -> U32:
  U32.add(U32.mul(x, 10), 4)

def run_loop(x: U32) -> U32:
  U32.add(U32.mul(x, 10), 5)

def main() -> IO(Unit):
  do IO<Unit>:
    t : Nat <- IO.now()
    IO.print(U32.show(run_loop(FOO_BAR(Foo.bar(foo_bar(foo.bar(U32.from_nat(Nat.mod(t, 1n)))))))))

#|12345
