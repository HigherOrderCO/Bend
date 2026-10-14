# each level uses the preceding one twice; compare the shared graph,
# not its 2^32 paths, in both the checker and the proven kernel (#1555)
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




def agree_32(+x: U32) -> {root_loop(32n, x) == root_rec(32n, x) : U32}:
  {==}

#|ALL PROOFS CHECK
#|Use --verdict for mathematical validity.
