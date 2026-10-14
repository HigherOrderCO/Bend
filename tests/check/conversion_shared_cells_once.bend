# two share cells found equal become one, so a shared graph is compared
# once: each level of both roots uses the level below twice, and walking
# the 16 levels as a tree would take 2^16 steps (#1151's gist); 16, not
# 32, because BendTT doesn't share cells yet and runs out of fuel (#1555)
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




def agree_16(+x: U32) -> {root_loop(16n, x) == root_rec(16n, x) : U32}:
  {==}

#|ALL PROOFS CHECK
#|Use --verdict for mathematical validity.
