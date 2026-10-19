# A trie character has a presence bit followed by 32 MSB-first code bits.
# Shared inputs must be returned intact, including non-Unicode U32 codes.
import Base

def check(ok: Bool) -> IO(Unit):
  match ok:
    case True{}:
      IO.pure(Unit, Unit{})
    case False{}:
      IO.die(Unit, 1, "wrong map bit or returned key")

def answer(want: String, bit: Bool, r: String & Bool) -> IO(Unit):
  (key, got) = r
  check(Bool.and(String.eq(want, key), Bool.not(Bool.xor(bit, got))))

def probe(+key: String, pos: Nat, bit: Bool) -> IO(Unit):
  answer(key, bit, Map.bit(key, pos))

def main() -> IO(Unit):
  do IO<Unit>:
    probe("", 0n, False{})
    probe("", Nat.add(Nat.mul(4294967295n, 65536n), 65535n), False{})
    probe("a", 0n, True{})
    probe("a", 1n, False{})
    probe("a", 26n, True{})
    probe("a", 27n, True{})
    probe("a", 31n, False{})
    probe("a", 32n, True{})
    probe("a", 33n, False{})
    probe("a", Nat.add(4294967295n, 1n), False{})
    probe("a", Nat.add(Nat.mul(4294967295n, 65536n), 65535n), False{})
    probe("\u{0}a", 0n, True{})
    probe("\u{0}a", 1n, False{})
    probe("\u{0}a", 32n, False{})
    probe("\u{0}a", 33n, True{})
    probe("\u{0}a", 59n, True{})
    probe("\u{0}a", 65n, True{})
    probe("\u{0}a", 66n, False{})
    probe("\u{ffffffff}", 0n, True{})
    probe("\u{ffffffff}", 1n, True{})
    probe("\u{ffffffff}", 32n, True{})
    probe("\u{ffffffff}", 33n, False{})
    probe("\u{80000000}", 1n, True{})
    probe("\u{80000000}", 32n, False{})
    probe("\u{7fffffff}", 1n, False{})
    probe("\u{7fffffff}", 32n, True{})
    IO.print("map bit boundaries and keys preserved")

#|map bit boundaries and keys preserved
