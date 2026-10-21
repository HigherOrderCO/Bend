# borrow witness (#1805): a word read from a slot that is a box elsewhere
# holds no reference. Hit puts a String and Flag a Bool at the same field,
# and Con lays out its head as a box, which a List<&2, U32> reads as a
# U32. hits, ons, sum and lens only read, so each borrows: its match
# reads in place (term_peek), not ctr_take. The Bool's empty label once
# made the slot's labels disagree (hits); a Bool read out of that slot
# (ons), a head sum keeps across its call, or a head lens boxes again in
# a list, once owned its root. main reads xs and ns after them, so a take
# would copy each cell.
import Base

type Ev is Data:
  Hit{key: String, value: String}
  Flag{key: String, on: Bool}

def evs() -> List<&2, Ev>:
  [Hit{"a", "1"}, Flag{"b", True{}}, Hit{"c", "2"}, Flag{"d", False{}}]

def bit(b: Bool) -> Nat:
  match b:
    case True{}: 1n
    case False{}: 0n

def hits(evs: List<&2, Ev>, n: Nat) -> Nat:
  match evs:
    case Nil{}: n
    case Hit{k, v} <> tail: hits(tail, 1n+n)
    case Flag{k, on} <> tail: hits(tail, n)

def ons(evs: List<&2, Ev>, n: Nat) -> Nat:
  match evs:
    case Nil{}: n
    case Hit{k, v} <> tail: ons(tail, n)
    case Flag{k, on} <> tail: ons(tail, Nat.add(bit(on), n))

def sum(ns: List<&2, U32>) -> U32:
  match ns:
    case Nil{}: 0
    case h <> tail: U32.add(h, sum(tail))

def lens(ns: List<&2, U32>) -> Nat:
  match ns:
    case Nil{}: 0n
    case h <> tail: Nat.add(List.length(&2, U32, [h]), lens(tail))

def main() -> IO(Unit):
  +xs = evs()
  +ns = {[3, 4, 5] : List<&2, U32>}
  a = hits(xs, 0n)
  b = ons(xs, 0n)
  c = U32.to_nat(sum(ns))
  d = lens(ns)
  IO.print(Nat.show(Nat.add(Nat.add(Nat.mul(10n, a), b),
    Nat.add(Nat.mul(100n, List.length(&2, Ev, xs)),
      Nat.mul(1000n, Nat.add(Nat.add(c, d), List.length(&2, U32, ns)))))))

#|18421
