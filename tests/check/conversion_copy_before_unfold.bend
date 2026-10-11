# two copies of one call convert before either unfolds: agree(32n, x)
# has the type written in agree_32, and unfolding both copies would walk
# a chain of 32 shared nodes as a tree of 2^32 (#1071, #1151)
import Base

def mix(a: U32, b: U32) -> U32:
  (a * 31 + b : U32)

def root_loop(d: Nat, +h: U32) -> U32:
  match d:
    case 0n: h
    case 1n+p: root_loop(p, mix(h, h))

def sq(+h: U32) -> U32:
  mix(h, h)

def root_rec(d: Nat, +x: U32) -> U32:
  match d:
    case 0n: x
    case 1n+p: sq(root_rec(p, x))

law slide:
  for d: Nat
  for +x: U32
  {root_loop(d, sq(x)) == sq(root_loop(d, x)) : U32}

def slide(d, x):
  match d:
    case 0n:
      {==}
    case 1n+p:
      slide(p, sq(x))

law agree:
  for +d: Nat
  for +x: U32
  {root_loop(d, x) == root_rec(d, x) : U32}

def agree(d, x):
  match d:
    case 0n:
      {==}
    case 1n+p:
      %agree(p, x) : {root_loop(p, mix(x, x)) == sq(_) : U32}
      slide(p, x)

def agree_32(+x: U32) -> {root_loop(32n, x) == root_rec(32n, x) : U32}:
  agree(32n, x)

#|ALL PROOFS CHECK
#|Use --verdict for mathematical validity.
