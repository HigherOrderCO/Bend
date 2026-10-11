# The foreign sink keeps a closure with a runtime array capture live, then
# drops it without calling it. Native arena growth stays within one
# allocation quantum after warm-up, including retained free lists.
import Base

law foo.go:
  for x: U32
  for r: Array<U32> & U32
  U32

def foo.go(x, r):
  (a2, v) = r
  U32.add(v, x)

law foo:
  for a: Array<U32>
  for x: U32
  U32

def foo(a, x):
  foo.go(x, Array.get(U32, a, 12))

def dispose(f: U32 -> U32) -> IO(U32):
  import "./closure_slab_drop.c"
  import "./closure_slab_drop.js"

def loop(n: Nat, +x: U32, last: U32) -> IO(U32):
  match n:
    case 0n:
      IO.pure(U32, last)
    case 1n+more:
      do IO<U32>:
        a : Array<U32> = [x : U32*16n]
        value : U32 <- dispose(foo(a))
        loop(more, x, value)

law main:
  IO(Unit)

def main():
  do IO<Unit>:
    xs : List<String> <- IO.args()
    x : U32 = U32.from_nat(List.length(&1, String, xs))
    value : U32 <- loop(20000n, x, 0)
    IO.print(U32.show(value))

#|5
