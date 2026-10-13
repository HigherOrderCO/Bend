# one lambda's code under captures of three layouts: each call gets a
# specialized def of its own, as a shared one would read a String or
# an F32 capture at the layout of a U32
import Base

def ap(-R: Type, x: U32, f: U32 -> R) -> R:
  f(x)

def lays(u: U32, s: String, r: F32) ->
  (U32 & U32) & (U32 & String) & U32 & F32:
  (ap(U32 & U32, 1, y => (y, u)), ap(U32 & String, 2, y => (y, s)),
    ap(U32 & F32, 3, y => (y, r)))

def main() -> (U32 & U32) & (U32 & String) & U32 & F32:
  lays(7, "hi", 1.5)

#|((1, 7), (2, "hi"), 3, 1.5)
