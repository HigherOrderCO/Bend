-- Native Lean twin of main.bend: a histogram of R fresh batches of 2^D
-- keys into K = 256 buckets, single-threaded, 1-to-1 with the Bend
-- program: per round, the same key stream (seed 7 + r) filled block by
-- block, the same two passes over the same 2^B x 256 table (pass 1
-- zeroes and counts each block's row; pass 2 sums the columns, in two
-- steps from 2^12 rows up; Arrays updated in place while uniquely
-- referenced), and the same fold of the round's counts.
def D : Nat := 24
def B : Nat := 11
def R : Nat := 128
def K : Nat := 256
def KB : Nat := 8

def word_prng (x : UInt32) : UInt32 :=
  let a := x ^^^ (x <<< (13 : UInt32))
  let b := a ^^^ (a >>> (17 : UInt32))
  b ^^^ (b <<< (5 : UInt32))

def key (s : UInt32) (j : Nat) : UInt32 :=
  word_prng (word_prng ((UInt32.ofNat j + 1) * 2654435761 + s)) % 256

-- pass 2: the columns, over the 2^d rows; from 2^12 rows up, each
-- column's sum over a group of 2^m rows into the group's last row, then
-- over the 2^r groups' last rows
def cols (d : Nat) (t0 : Array UInt32) : Array UInt32 × Array UInt32 := Id.run do
  let r := if d < 12 then d else d / 2
  let m := if d < 12 then 0 else d - r
  let mut t := t0
  if d ≥ 12 then
    for g in [0:2 ^ r] do
      let top := g * 2 ^ (m + KB)
      let last := top + (2 ^ m - 1) * 2 ^ KB
      for c in [0:K] do
        let mut s : UInt32 := 0
        for i in [0:2 ^ m] do
          s := s + t[top + i * 2 ^ KB + c]!
        t := t.set! (last + c) s
  let o := (2 ^ m - 1) * 2 ^ KB
  let mut cs : Array UInt32 := Array.replicate K 0
  for c in [0:K] do
    let mut s : UInt32 := 0
    for g in [0:2 ^ r] do
      s := s + t[o + g * 2 ^ (m + KB) + c]!
    cs := cs.set! c s
  return (t, cs)

-- one count: pass 1 per block (its row zeroed, then its 2^e keys counted
-- into it), then pass 2
def count (d e : Nat) (ks : Array UInt32) (t0 : Array UInt32) :
    Array UInt32 × Array UInt32 := Id.run do
  let mut t := t0
  for i in [0:2 ^ d] do
    let row := i * 2 ^ KB
    for c in [0:K] do
      t := t.set! (row + c) 0
    for j in [i * 2 ^ e:(i + 1) * 2 ^ e] do
      let b := row + (min ks[j]! (UInt32.ofNat K - 1)).toNat
      t := t.set! b (t[b]! + 1)
  return cols d t

def main : IO Unit := do
  let d := min B D
  let mut ks : Array UInt32 := Array.replicate (2 ^ D) 0
  let mut t : Array UInt32 := Array.replicate (2 ^ (d + KB)) 0
  let mut h : UInt32 := 0
  for n in [0:R] do
    for j in [0:2 ^ D] do
      ks := ks.set! j (key (7 + UInt32.ofNat n) j)
    let (t2, cs) := count d (D - d) ks t
    t := t2
    for c in [0:K] do
      h := (h * 2654435761) ^^^ cs[c]!
  IO.println h
