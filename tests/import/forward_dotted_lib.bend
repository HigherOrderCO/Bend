# a def names a dotted datatype and constructor declared below it. an
# imported file resolved Cell.At<p>'s head and the Cell.Mk arm's name
# twice, into lib:lib:Cell.At, a key no file declares (#1514)
import Base

def Fam(n: Nat) -> Data:
  match n:
    case 0n:
      Unit
    case 1n+p:
      Cell.At<p>

def get(p: Nat, c: Fam(1n+p)) -> U32:
  {\{Cell.Mk: v => v} : Cell.At<p> -> U32}(c)

type Cell.At<-p: Nat> is Data:
  Cell.Mk{v: U32}

def main() -> U32:
  get(2n, Cell.Mk{7})

#|7
