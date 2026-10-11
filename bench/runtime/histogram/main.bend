import Base
# histogram: a parallel histogram of 2^D U32 keys into K = 256 buckets
# over one flat Array<U32>, with no atomics -- the suite's shared-array,
# multi-pass case. The histogram is bend-parallel's histogram.bend
# (github.com/costamatheus97/bend-parallel at 45a161a, Apache-2.0, by
# costamatheus97), ported as is but for its sort-only path (the sc flag
# and ins.go), dropped, and Scan.half, inlined as half.
#
# A run histograms R fresh batches of 2^D keys in the same key array and
# buffers. Round r:
# - fill: the keys are cut into 2^B blocks; each block writes its own
#   slots, key j = prng(prng((j + 1) * 2654435761 + 7 + r)) % 256 (round
#   0 is the stream of bend-parallel's tests and bench);
# - count, pass 1: each block zeroes its row of a 2^B x 256 table and
#   counts its keys into it;
# - count, pass 2: a bucket's count is the sum of its column. From 2^12
#   rows up the rows are cut into 2^(B/2) groups: each (group, bucket)
#   sums into the group's last row, then each bucket sums the groups;
# - fold: the round's 256 counts, in order, into the checksum, h =
#   h * 2654435761 ^ c (bend-parallel's hash), carried across rounds.
# The key array and the buffers are sized once outside the `!`
# (Array.new fills on one lane). Knobs: D (2^D keys), B (the fork depth:
# 2^B blocks, every B gives the same counts) and R.
#
# Unsafe: the lanes share the keys and the table through Base's @unsafe
# Array.fork and Array.join, as bend-parallel does, so --check-only says
# which defs rely on them. Every pass gives each lane its own slots.
#
# Sizes: small D = 10n, B = 4n, R = 2n (expect 3825188900, verified
# against the C twin), big D = 24n, B = 11n, R = 128n (expect
# 122845198).

# floor(d / 2).
def half(+d: Nat) -> Nat:
  match d:
    case 0n:
      0n
    case 1n+p:
      match p:
        case 0n:
          0n
        case 1n+q:
          1n+half(q)

# Buffers
# -------

type Bufs is Type:
  Bufs{counts: Array<U32>, table: Array<U32>}

def kbits.of(big: Bool, +k: U32) -> Nat:
  match big:
    case False{}:
      0n
    case True{}:
      1n+U32.log2((k - 1 : U32))

# ceil(log2 K), for K >= 1.
def kbits(+k: U32) -> Nat:
  kbits.of(U32.is_lt(1, k), k)

# Buffers for 2^lg keys at fork depth b and k buckets, allocated now.
def Bufs.sized(+b: Nat, +k: U32, +lg: Nat) -> Bufs:
  +kb = kbits(U32.max(k, 1))
  Bufs{Array.new(U32, kb, 0), Array.new(U32, Nat.add(Nat.min(b, lg), kb), 0)}

def fit.pick(ok: Bool, +d: Nat, t: Array<U32>) -> Array<U32>:
  match ok:
    case True{}:
      t
    case False{}:
      Array.new(U32, d, 0)

# t when it holds exactly 2^d entries, else a new one.
def fit.eq(+d: Nat, r: Array<U32> & U32) -> Array<U32>:
  (t, +n) = r
  fit.pick(U32.is_eq(n, U32.shln(1, d)), d, t)

# t when it holds at least 2^d entries, else a new one.
def fit.ge(+d: Nat, r: Array<U32> & U32) -> Array<U32>:
  (t, +n) = r
  fit.pick(U32.is_ge(n, U32.shln(1, d)), d, t)

# Handles
# -------

type Two is Type:
  Two{a: Array<U32>, b: Array<U32>}

def two.join(l: Two, r: Two) -> Two:
  Two{la, lb} = l
  Two{ra, rb} = r
  Two{Array.join(U32, la, ra), Array.join(U32, lb, rb)}

def two.rejoin(a2: Array<U32>, b2: Array<U32>, r: Two) -> Two:
  Two{a, b} = r
  Two{Array.join(U32, a, a2), Array.join(U32, b, b2)}

# Pass 1: rows
# ------------

# n + 1 zeros from at.
def zero.go(+n: Nat, +at: U32, t: Array<U32>) -> Array<U32>:
  match n:
    case 0n:
      t[at] <- 0
    case 1n+p:
      zero.go(p, (at + 1 : U32), t[at] <- 0)

def bump.at(+i: U32, r: Array<U32> & U32) -> Array<U32>:
  (t, +v) = r
  t[i] <- (v + 1 : U32)

# One more at t[i].
def bump(+i: U32, t: Array<U32>) -> Array<U32>:
  bump.at(i, t[i])

# r holds the keys and the key x read at j: one more in x's bucket of the
# row at row, then the same for the n keys after j. k1 is K - 1.
def count.go(+n: Nat, +j: U32, +row: U32, +k1: U32, t: Array<U32>, r: Array<U32> & U32) -> Two:
  match n:
    case 0n:
      (ks, +x) = r
      Two{ks, bump((row + U32.min(x, k1) : U32), t)}
    case 1n+p:
      (ks, +x) = r
      +q = (j + 1 : U32)
      count.go(p, q, row, k1, bump((row + U32.min(x, k1) : U32), t), ks[q])

# Block i (2^e keys from i * 2^e): its row, at i * 2^kb, zeroed, then its
# keys counted into it. k is K (at least 1).
def rows.leaf(+i: U32, +e: Nat, +kb: Nat, +k: U32, ks: Array<U32>, t: Array<U32>) -> Two:
  +row = U32.shln(i, kb)
  +at = U32.shln(i, e)
  count.go(U32.to_nat((U32.shln(1, e) - 1 : U32)), at, row, (k - 1 : U32),
    zero.go(U32.to_nat((k - 1 : U32)), row, t), ks[at])

# Pass 1's forks: d levels down to the blocks under block index i. fk and
# ft are two handles each to the keys and the table. Block i reads only
# its keys and reads and writes only its row (entries i * 2^kb to
# i * 2^kb + K - 1): no two lanes touch one slot.
def rows(+d: Nat, +i: U32, +e: Nat, +kb: Nat, +k: U32, fk: Array<U32> & Array<U32>, ft: Array<U32> & Array<U32>) -> Two:
  match d:
    case 0n:
      (ks, ks2) = fk
      (t, t2) = ft
      two.rejoin(ks2, t2, rows.leaf(i, e, kb, k, ks, t))
    case 1n+p:
      (k0, k1) = fk
      (t0, t1) = ft
      +j = (i * 2 : U32)
      l r = rows(p, j, e, kb, k, Array.fork(U32, k0), Array.fork(U32, t0))
        rows(p, (j + 1 : U32), e, kb, k, Array.fork(U32, k1), Array.fork(U32, t1))
      two.join(l, r)

# Pass 2: columns
# ---------------

# From 2^12 rows up (see small, below), the table's 2^d rows are cut into
# 2^r groups of 2^m (r = d / 2): the column passes step over the rows of
# a group, then over the groups. Below, one pass steps over all the rows:
# on the GPU we measured, the split costs more than it saves there.

# d is below 12: the table is small, and its rows are not split.
def small(+d: Nat) -> Bool:
  U32.is_lt(U32.from_nat(d), 12)

# r holds the table and the entry x read at at: adds x and the n entries
# after it, each step further on, to s.
def col.go(+n: Nat, +at: U32, +step: U32, +s: U32, r: Array<U32> & U32) -> Array<U32> & U32:
  match n:
    case 0n:
      (t, +x) = r
      (t, (s + x : U32))
    case 1n+p:
      (t, +x) = r
      +q = (at + step : U32)
      col.go(p, q, step, (s + x : U32), t[q])

def grp.put(+at: U32, r: Array<U32> & U32) -> Array<U32>:
  (t, +v) = r
  t[at] <- v

# Column c of a group (rows from top, nm + 1 of them, step apart), when
# c < K: the sum goes into the last row (at last).
def grp.one(lt: Bool, +c: U32, +top: U32, +last: U32, +step: U32, +nm: Nat, t: Array<U32>) -> Array<U32>:
  match lt:
    case True{}:
      grp.put((last + c : U32), col.go(nm, (top + c : U32), step, 0, t[(top + c : U32)]))
    case False{}:
      t

# Columns c to c + n of a group.
def grp.go(+n: Nat, +c: U32, +k: U32, +top: U32, +last: U32, +step: U32, +nm: Nat, t: Array<U32>) -> Array<U32>:
  match n:
    case 0n:
      grp.one(U32.is_lt(c, k), c, top, last, step, nm, t)
    case 1n+q:
      grp.go(q, (c + 1 : U32), k, top, last, step, nm, grp.one(U32.is_lt(c, k), c, top, last, step, nm, t))

# Lane i: rows group i / 2^gc (2^m rows of 2^kb), columns group i % 2^gc
# (2^w columns).
def grp.leaf(+i: U32, +gc: Nat, +w: Nat, +m: Nat, +kb: Nat, +k: U32, t: Array<U32>) -> Array<U32>:
  +g = U32.shrn(i, gc)
  +j = (i - U32.shln(g, gc) : U32)
  +top = U32.shln(g, Nat.add(m, kb))
  +last = (top + U32.shln((U32.shln(1, m) - 1 : U32), kb) : U32)
  grp.go(U32.to_nat((U32.shln(1, w) - 1 : U32)), U32.shln(j, w), k, top, last, U32.shln(1, kb),
    U32.to_nat((U32.shln(1, m) - 1 : U32)), t)

# Pass 2a's forks: d levels down to the lanes
# under lane i, with two handles to the table. Lane i reads and writes
# only its group's rows in its columns: no two lanes touch one slot.
def grp(+d: Nat, +i: U32, +gc: Nat, +w: Nat, +m: Nat, +kb: Nat, +k: U32, ft: Array<U32> & Array<U32>) -> Array<U32>:
  match d:
    case 0n:
      (t, t2) = ft
      Array.join(U32, grp.leaf(i, gc, w, m, kb, k, t), t2)
    case 1n+p:
      (t0, t1) = ft
      +j = (i * 2 : U32)
      l r = grp(p, j, gc, w, m, kb, k, Array.fork(U32, t0))
        grp(p, (j + 1 : U32), gc, w, m, kb, k, Array.fork(U32, t1))
      Array.join(U32, l, r)

def col.put(+c: U32, cs: Array<U32>, r: Array<U32> & U32) -> Two:
  (t, +v) = r
  Two{t, cs[c] <- v}

# Column c's count into cs[c]: the sum of its nb + 1 entries from o + c,
# step apart (the groups' last rows), when c < K; else 0.
def col.one(lt: Bool, +c: U32, +o: U32, +step: U32, +nb: Nat, p: Two) -> Two:
  match lt:
    case True{}:
      Two{t, cs} = p
      col.put(c, cs, col.go(nb, (o + c : U32), step, 0, t[(o + c : U32)]))
    case False{}:
      Two{t, cs} = p
      Two{t, cs[c] <- 0}

# Columns c to c + n.
def cols.go(+n: Nat, +c: U32, +k: U32, +o: U32, +step: U32, +nb: Nat, p: Two) -> Two:
  match n:
    case 0n:
      col.one(U32.is_lt(c, k), c, o, step, nb, p)
    case 1n+q:
      cols.go(q, (c + 1 : U32), k, o, step, nb, col.one(U32.is_lt(c, k), c, o, step, nb, p))

# Pass 2b's forks: d levels down to groups of 2^w columns under group index
# j. ft and fc are two handles each to the table and the counts. Group j
# reads only its columns of the table and writes only its counts (entries
# j * 2^w to j * 2^w + 2^w - 1): no two lanes write one slot.
def cols(+d: Nat, +j: U32, +w: Nat, +k: U32, +o: U32, +step: U32, +nb: Nat, ft: Array<U32> & Array<U32>, fc: Array<U32> & Array<U32>) -> Two:
  match d:
    case 0n:
      (t, t2) = ft
      (cs, cs2) = fc
      two.rejoin(t2, cs2, cols.go(U32.to_nat((U32.shln(1, w) - 1 : U32)), U32.shln(j, w), k, o, step, nb, Two{t, cs}))
    case 1n+p:
      (t0, t1) = ft
      (c0, c1) = fc
      +i = (j * 2 : U32)
      l r = cols(p, i, w, k, o, step, nb, Array.fork(U32, t0), Array.fork(U32, c0))
        cols(p, (i + 1 : U32), w, k, o, step, nb, Array.fork(U32, t1), Array.fork(U32, c1))
      two.join(l, r)

# Driver
# ------

def count.fin(keys: Array<U32>, r: Two) -> Array<U32> & Bufs:
  Two{table, counts} = r
  (keys, Bufs{counts, table})

# Pass 2b over the 2^kb columns, 2^min(d, kb) groups: each sums its
# columns over the 2^r groups' last rows.
def count.cols(+r: Nat, +m: Nat, +g: Nat, +k: U32, +kb: Nat, keys: Array<U32>, counts: Array<U32>, table: Array<U32>) -> Array<U32> & Bufs:
  count.fin(keys, cols(g, 0, Nat.sub(kb, g), k, U32.shln((U32.shln(1, m) - 1 : U32), kb), U32.shln(1, Nat.add(m, kb)),
    U32.to_nat((U32.shln(1, r) - 1 : U32)), Array.fork(U32, table), Array.fork(U32, fit.eq(kb, Array.size(U32, counts)))))

# Pass 2a over 2^r groups of 2^m rows by 2^min(d, kb) groups of columns,
# then 2b; below 2^12 rows, 2b alone, over 2^d groups of one row.
def count.grp(lo: Bool, +d: Nat, +k: U32, +kb: Nat, counts: Array<U32>, p: Two) -> Array<U32> & Bufs:
  match lo:
    case True{}:
      Two{keys, table} = p
      count.cols(d, 0n, Nat.min(d, kb), k, kb, keys, counts, table)
    case False{}:
      Two{keys, table} = p
      +g = Nat.min(d, kb)
      +r = half(d)
      +m = Nat.sub(d, r)
      count.cols(r, m, g, k, kb, keys, counts, grp(Nat.add(r, g), 0, g, Nat.sub(kb, g), m, kb, k, Array.fork(U32, table)))

def count.at(+b: Nat, +k: U32, bufs: Bufs, r: Array<U32> & U32) -> Array<U32> & Bufs:
  Bufs{counts, table} = bufs
  (keys, +n) = r
  +kk = U32.max(k, 1)
  +kb = kbits(kk)
  +lg = U32.log2(n)
  +d = Nat.min(b, lg)
  count.grp(small(d), d, kk, kb, counts, rows(d, 0, Nat.sub(lg, d), kb, kk, Array.fork(U32, keys),
    Array.fork(U32, fit.ge(Nat.add(d, kb), Array.size(U32, table)))))

# API
# ---

# The histogram of keys into k buckets at fork depth b. Returns the keys,
# unchanged, and the buffers, whose counts field holds the counts.
def count(+b: Nat, +k: U32, keys: Array<U32>, bufs: Bufs) -> Array<U32> & Bufs:
  count.at(b, k, bufs, Array.size(U32, keys))

# Keys
# ----

# xorshift32 (13, 17, 5).
def prng(+x: U32) -> U32:
  +a = U32.xor(x, U32.shln(x, 13n))
  +b = U32.xor(a, U32.shrn(a, 17n))
  U32.xor(b, U32.shln(b, 5n))

# Key j of stream s.
def key(+s: U32, +j: U32) -> U32:
  U32.mod(prng(prng(((j + 1) * 2654435761 + s : U32))), 256)

# Keys j to j + n of stream s.
def fill.go(+n: Nat, +j: U32, +s: U32, ks: Array<U32>) -> Array<U32>:
  match n:
    case 0n:
      ks[j] <- key(s, j)
    case 1n+p:
      fill.go(p, (j + 1 : U32), s, ks[j] <- key(s, j))

# The fill's forks: d levels down to the blocks of 2^e keys under block
# i, with two handles to the keys. Block i writes only its keys.
def fill(+d: Nat, +i: U32, +e: Nat, +s: U32, fk: Array<U32> & Array<U32>) -> Array<U32>:
  match d:
    case 0n:
      (ks, ks2) = fk
      Array.join(U32, fill.go(U32.to_nat((U32.shln(1, e) - 1 : U32)), U32.shln(i, e), s, ks), ks2)
    case 1n+p:
      (k0, k1) = fk
      +j = (i * 2 : U32)
      l r = fill(p, j, e, s, Array.fork(U32, k0)) fill(p, (j + 1 : U32), e, s, Array.fork(U32, k1))
      Array.join(U32, l, r)

# Run
# ---

# r holds the counts and the count x read at j: h folds x, then the n
# counts after j.
def sum.go(+n: Nat, +j: U32, +h: U32, r: Array<U32> & U32) -> Array<U32> & U32:
  match n:
    case 0n:
      (cs, +x) = r
      (cs, U32.xor((h * 2654435761 : U32), x))
    case 1n+p:
      (cs, +x) = r
      +q = (j + 1 : U32)
      sum.go(p, q, U32.xor((h * 2654435761 : U32), x), cs[q])

# A round's state: the keys, the buffers and the checksum so far.
type St is Type:
  St{keys: Array<U32>, bufs: Bufs, h: U32}

def fold.fin(keys: Array<U32>, table: Array<U32>, r: Array<U32> & U32) -> St:
  (counts, +h) = r
  St{keys, Bufs{counts, table}, h}

# The round's counts folded into h.
def fold(+h: U32, r: Array<U32> & Bufs) -> St:
  (keys, bufs) = r
  Bufs{counts, table} = bufs
  fold.fin(keys, table, sum.go(255n, 0, h, counts[0]))

# n more rounds from stream s, at fork depth b over blocks of 2^e keys.
def reps(+n: Nat, +s: U32, +b: Nat, +e: Nat, st: St) -> U32:
  match n:
    case 0n:
      St{keys, bufs, +h} = st
      h
    case 1n+p:
      St{keys, bufs, +h} = st
      reps(p, (s + 1 : U32), b, e, fold(h, count(b, 256, fill(b, 0, e, s, Array.fork(U32, keys)), bufs)))

# n rounds over 2^d keys at fork depth b.
def run(+n: Nat, +d: Nat, +b: Nat, keys: Array<U32>, bufs: Bufs) -> U32:
  +fb = Nat.min(b, d)
  reps(n, 7, fb, Nat.sub(d, fb), St{keys, bufs, 0})

def size() -> Nat:
  24n

def depth() -> Nat:
  11n

def rounds() -> Nat:
  128n

# small: D = 10n, B = 4n, R = 2n expect 3825188900; big: D = 24n, B = 11n,
# R = 128n expect 122845198
def main() -> IO(Unit):
  keys = Array.new(U32, size(), 0)
  bufs = Bufs.sized(depth(), 256, size())
  IO.print(U32.show(run!(rounds(), size(), depth(), keys, bufs)))
