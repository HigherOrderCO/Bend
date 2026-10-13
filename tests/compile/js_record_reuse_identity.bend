# JS reference identity checks that record reuse occurs.
# js_record_reuse.bend checks values on both backends.
# Identity is not a Bend language rule.
import Base
import ./js_record_reuse.bend as R

type Ops is Type:
  Ops{
    keep: R.Point -> R.Point,
    nested: R.Box -> R.Box,
    changed: R.Point -> R.Point,
    swapped: R.Point -> R.Point,
    cross: R.Point -> R.Point -> R.Point,
    nested_change: R.Box -> R.Box,
    choice: R.Choice -> R.Choice,
    retag: R.Choice -> R.Choice,
    hold: R.Point -> R.Holder,
    hold2: R.Point -> R.Holder2,
    hold_box: R.Box -> R.BoxHolder,
    indexed: R.Indexed<5n> -> R.Indexed<5n>,
    item: R.Item<U32> -> R.Item<U32>,
    empty: R.EmptyRecord -> R.EmptyRecord
  }

def verify(ops: Ops, p: R.Point, q: R.Point, b: R.Box,
  c: R.Choice, right: R.Choice, r: R.Indexed<5n>,
  i: R.Item<U32>, e: R.EmptyRecord) -> IO(Unit):
  import "./js_record_reuse_identity.js"

def main() -> IO(Unit):
  do IO<Unit>:
    +p : R.Point = R.Point{3, 9}
    Unit <- verify(Ops{p => R.keep(p), b => R.nested(b), R.changed,
      R.swapped, R.cross, R.nested_change, c => R.choice(c), R.retag,
      p => R.hold(p), p => R.hold2(p), b => R.hold_box(b),
      r => R.indexed(5n, r), i => R.item(U32, i), e => R.empty(e)},
      p, R.Point{10, 20}, R.Box{p, 57}, R.Left{p}, R.Right{p},
      R.Indexed{7n, 5}, R.Item{9}, R.EmptyRecord{})
    IO.print("ok")

#|ok
