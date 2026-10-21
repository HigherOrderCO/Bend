import Base

type T is Data:
  K{a: U32, b: U32, c: U32}

type R is Data:
  A{a: U32, b: U32, c: U32}
  B{y: T}

law main:
  for a: T
  R

def main(a):
  match a:
    case K{a,b,c}:
      A{a,b,c}
    case a:
      B{a}

#|\{K: (a => b => c => A{a, b, c})}
