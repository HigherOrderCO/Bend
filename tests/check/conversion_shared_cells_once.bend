# two share cells found equal become one, so a shared graph is compared
# once: each level of both roots uses the level below twice, and walking
# the 32 levels as a tree would take 2^32 steps (#1151's gist)
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
