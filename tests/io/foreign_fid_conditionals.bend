# Presence guards must not root functions; macro-expanded calls must.
import Base

def guard_only(n: U32) -> U32:
  U32.inc(n)

def imul(a: U32, b: U32) -> U32:
  (a * b : U32)

def answer() -> IO(U32):
  import "./foreign_fid_conditionals.c"
  import "./foreign_fid_conditionals.js"

def main() -> IO(Unit):
  do IO<Unit>:
    value : U32 <- answer()
    IO.print(U32.show(value))

#|42
