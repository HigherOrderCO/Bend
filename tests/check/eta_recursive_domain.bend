# Recursive function domains have no finite eta-long form. The kernel
# accepts the neutral argument when elaboration reaches the same type again.
import Base

def F() -> Type:
  @-x: F -> Empty

def consume(f: @-x: F -> Empty) -> Empty:
  f(f)

def keep(f: @-x: F -> Empty) -> @-x: F -> Empty:
  f

def G() -> Type:
  (G -> U32) -> U32

def run(g: G, k: G -> U32) -> U32:
  g(k)

# Original arguments of a finite alias still need eta for equality.
def Fn() -> Type:
  U32 -> U32

def identity(x: U32) -> U32:
  x

def choose(f: Fn, -g: Fn, -same: {f == g : Fn}) -> Fn:
  f

def original_args() -> Fn:
  choose(identity, x => x, {==})

def main() -> U32:
  original_args(42)

#|42
