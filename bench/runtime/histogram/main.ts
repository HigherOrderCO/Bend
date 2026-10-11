// Native TypeScript twin of main.bend: a histogram of R fresh batches of
// 2^D keys into K = 256 buckets, single-threaded, 1-to-1 with the Bend
// program: per round, the same key stream (seed 7 + r) filled block by
// block, the same two passes over the same 2^B x 256 Uint32Array table
// (pass 1 zeroes and counts each block's row; pass 2 sums the columns,
// in two steps from 2^12 rows up), and the same fold of the round's
// counts (imul + >>>0 for wrapping u32).
const D = 24;
const B = 11;
const R = 128;
const K = 256;
const KB = 8;

function word_prng(x: number): number {
  const a = (x ^ (x << 13)) >>> 0;
  const b = (a ^ (a >>> 17)) >>> 0;
  return (b ^ (b << 5)) >>> 0;
}

function key(s: number, j: number): number {
  return word_prng(word_prng((Math.imul(j + 1, 2654435761) + s) >>> 0)) % K;
}

// pass 2: the columns, over the 2^d rows; from 2^12 rows up, each
// column's sum over a group of 2^m rows into the group's last row, then
// over the 2^r groups' last rows
function cols(d: number, t: Uint32Array, cs: Uint32Array): void {
  const r = d < 12 ? d : d >>> 1;
  const m = d < 12 ? 0 : d - r;
  if (d >= 12) {
    for (let g = 0; g < 1 << r; g++) {
      const top = g << (m + KB);
      const last = top + (((1 << m) - 1) << KB);
      for (let c = 0; c < K; c++) {
        let s = 0;
        for (let i = 0; i < 1 << m; i++) {
          s = (s + t[top + (i << KB) + c]) >>> 0;
        }
        t[last + c] = s;
      }
    }
  }
  const o = ((1 << m) - 1) << KB;
  for (let c = 0; c < K; c++) {
    let s = 0;
    for (let g = 0; g < 1 << r; g++) {
      s = (s + t[o + (g << (m + KB)) + c]) >>> 0;
    }
    cs[c] = s;
  }
}

// one count: pass 1 per block (its row zeroed, then its 2^e keys counted
// into it), then pass 2
function count(d: number, e: number, ks: Uint32Array, t: Uint32Array,
  cs: Uint32Array): void {
  for (let i = 0; i < 1 << d; i++) {
    const row = i << KB;
    for (let c = 0; c < K; c++) {
      t[row + c] = 0;
    }
    for (let j = i << e; j < (i + 1) << e; j++) {
      t[row + Math.min(ks[j], K - 1)]++;
    }
  }
  cols(d, t, cs);
}

const d = Math.min(B, D);
const ks = new Uint32Array(1 << D);
const t = new Uint32Array(1 << (d + KB));
const cs = new Uint32Array(K);
let h = 0;
for (let n = 0; n < R; n++) {
  for (let j = 0; j < 1 << D; j++) {
    ks[j] = key(7 + n, j);
  }
  count(d, D - d, ks, t, cs);
  for (let c = 0; c < K; c++) {
    h = (Math.imul(h, 2654435761) ^ cs[c]) >>> 0;
  }
}
console.log(h);
