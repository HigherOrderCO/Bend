# Each root builds its own shared graph. Cache keys may be distinct outer
# nodes over the same child; neither equality nor lookup may walk a 2^32
# tree through these identity wrappers (#1193).
import Base


def keep_word(x: U32) -> U32:
  x

def mix(a: U32, b: U32) -> U32:
  (a * 31 + b : U32)

def root_loop(d: Nat, +h: U32) -> U32:
  match d:
    case 0n: h
    case 1n+p: root_loop(p, mix(keep_word(h), keep_word(h)))

def other_loop(d: Nat, +h: U32) -> U32:
  match d:
    case 0n: h
    case 1n+p: other_loop(p, mix(keep_word(h), keep_word(h)))




def agree_32(+x: U32) -> {root_loop(32n, x) == other_loop(32n, x) : U32}:
  {==}

#|ALL PROOFS CHECK
#|Use --verdict for mathematical validity.
