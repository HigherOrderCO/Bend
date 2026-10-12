# drop order through an Array: term_drop frees an Array's frame once only
# trivial cells follow, so a chain whose spine runs through one cell frees
# each Array before it drops the next link. ACon moves its kids behind its
# item. A 4-cell Array with the child in cell 1 becomes a plain frame of 2;
# a 512-cell one stays an Array frame, with the child last (512) and in
# cell 255 (live end 256, one past a plain frame). Each round drops fresh
# chains whole, then sums fresh ones; a cell freed early would read back
# wrong from reused memory in a later round. Expect 3 rounds of 3 * 5050.
import Base

type AT is Type:
  ANil{}
  ACon{kids: Array<AT>, item: List<&2, U32>}

def first(xs: List<&2, U32>) -> U32:
  match xs:
    case []:
      0
    case x <> rest:
      x

def nils(+d: Nat) -> Array<AT>:
  match d:
    case 0n:
      ALeaf{ANil{}}
    case 1n+p:
      ANode{nils(p), nils(p)}

# the child in the last cell, or with mid in the last cell of the low half
def kids(+d: Nat, mid: Bool, t: AT) -> Array<AT>:
  match d mid:
    case 0n _:
      ALeaf{t}
    case 1n+p True{}:
      ANode{kids(p, False{}, t), nils(p)}
    case 1n+p False{}:
      ANode{nils(p), kids(p, False{}, t)}

def at(+d: Nat, +mid: Bool, +n: Nat) -> AT:
  match n:
    case 0n:
      ANil{}
    case 1n+m:
      ACon{kids(d, mid, at(d, mid, m)), [U32.from_nat(n)]}

def pick(a: Array<AT>, mid: Bool) -> AT:
  match a mid:
    case ALeaf{t} _:
      t
    case ANode{xs, ys} True{}:
      pick(xs, False{})
    case ANode{xs, ys} False{}:
      pick(ys, False{})

def at_sum(+n: Nat, +mid: Bool, t: AT) -> U32:
  match n t:
    case 0n _:
      0
    case _ ANil{}:
      0
    case 1n+m ACon{ks, item}:
      U32.add(first(item), at_sum(m, mid, pick(ks, mid)))

def waste(a: AT, b: AT, c: AT, x: U32) -> U32:
  x

def round(+n: Nat) -> U32:
  a = waste(at(2n, True{}, n), at(9n, False{}, n), at(9n, True{}, n), 0)
  b = U32.add(at_sum(n, True{}, at(2n, True{}, n)), at_sum(n, False{}, at(9n, False{}, n)))
  U32.add(a, U32.add(b, at_sum(n, True{}, at(9n, True{}, n))))

def main() -> IO(Unit):
  IO.print(Nat.show(U32.to_nat(round(100n)))
    ++ " " ++ Nat.show(U32.to_nat(round(100n)))
    ++ " " ++ Nat.show(U32.to_nat(round(100n))))

#|15150 15150 15150
