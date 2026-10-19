# Structurally equal arm bodies with captures of different layouts:
# `f(x)` captures a closure and a U32 word, `h(p)` a closure and a String
# (boxed) one. A share that ignored the captured environment would hand a
# boxed word to the U32 spin's parameter. Each scope still shares its own
# duplicated default internally.
import Base

def scope_u32(n: Nat, f: U32 -> U32, x: U32) -> U32:
  match n:
    case 3n:
      f(x)
    case _:
      f(x)

def scope_text(n: Nat, h: String -> U32, p: String) -> U32:
  match n:
    case 3n:
      h(p)
    case _:
      h(p)

law main:
  IO(Unit)

def main():
  do IO<Unit>:
    IO.print(U32.show(scope_u32(1n, a => U32.add(a, 1), 41)))
    IO.print(U32.show(scope_text(1n, s => U32.from_nat(String.length(s)), "four")))

#|42
#|4
