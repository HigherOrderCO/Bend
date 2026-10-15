# lifted closures retain the operands of U32, Char and F32 match VIEWs
import Base

type Box is Type:
  H{f: U32 -> U32}

type FBox is Type:
  F{f: F32 -> F32}

def word(n: U32) -> Box:
  match n:
    case U32{bits}:
      H{x => U32.add(U32{bits}, x)}

def char(c: Char) -> Box:
  match c:
    case Chr{n}:
      H{x => U32.add(n, x)}

def float(n: F32) -> FBox:
  match n:
    case F32{bits}:
      F{x => F32.add(F32{bits}, x)}

def use(box: Box, x: U32) -> U32:
  match box:
    case H{f}: f(x)

def usef(box: FBox, x: F32) -> F32:
  match box:
    case F{f}: f(x)

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print(U32.show(use(word(42), 9)))
    IO.print(U32.show(use(char('Q'), 9)))
    IO.print(F32.show(usef(float(2.5), 1.25)))

#|51
#|90
#|3.75
