// Native C twin of main.bend: a histogram of R fresh batches of 2^D keys
// into K = 256 buckets, single-threaded, 1-to-1 with the Bend program:
// per round, the same key stream (seed 7 + r) filled block by block, the
// same two passes over the same 2^B x 256 table (pass 1 zeroes and
// counts each block's row; pass 2 sums the columns, in two steps from
// 2^12 rows up), and the same fold of the round's counts.
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#ifndef D
#define D 24u
#endif
#ifndef B
#define B 11u
#endif
#ifndef R
#define R 128u
#endif
#define K 256u
#define KB 8u

static uint32_t word_prng(uint32_t x) {
  uint32_t a = x ^ (x << 13u);
  uint32_t b = a ^ (a >> 17u);
  return b ^ (b << 5u);
}

static uint32_t key(uint32_t s, uint32_t j) {
  return word_prng(word_prng((j + 1u) * 2654435761u + s)) % K;
}

// pass 2: the columns, over the 2^d rows; from 2^12 rows up, each
// column's sum over a group of 2^m rows into the group's last row, then
// over the 2^r groups' last rows
static void cols(uint32_t d, uint32_t *t, uint32_t *cs) {
  uint32_t r = d < 12u ? d : d / 2u;
  uint32_t m = d < 12u ? 0u : d - r;
  if (d >= 12u) {
    for (uint32_t g = 0u; g < (1u << r); ++g) {
      uint32_t top = g << (m + KB);
      uint32_t last = top + (((1u << m) - 1u) << KB);
      for (uint32_t c = 0u; c < K; ++c) {
        uint32_t s = 0u;
        for (uint32_t i = 0u; i < (1u << m); ++i) {
          s += t[top + (i << KB) + c];
        }
        t[last + c] = s;
      }
    }
  }
  uint32_t o = ((1u << m) - 1u) << KB;
  for (uint32_t c = 0u; c < K; ++c) {
    uint32_t s = 0u;
    for (uint32_t g = 0u; g < (1u << r); ++g) {
      s += t[o + (g << (m + KB)) + c];
    }
    cs[c] = s;
  }
}

// one count: pass 1 per block (its row zeroed, then its 2^e keys counted
// into it), then pass 2
static void count(uint32_t d, uint32_t e, const uint32_t *ks, uint32_t *t,
                  uint32_t *cs) {
  for (uint32_t i = 0u; i < (1u << d); ++i) {
    uint32_t *row = t + (i << KB);
    for (uint32_t c = 0u; c < K; ++c) {
      row[c] = 0u;
    }
    for (uint32_t j = i << e; j < (i + 1u) << e; ++j) {
      row[ks[j] < K - 1u ? ks[j] : K - 1u] += 1u;
    }
  }
  cols(d, t, cs);
}

int main(void) {
  uint32_t d = B < D ? B : D;
  uint32_t *ks = calloc(1u << D, sizeof(uint32_t));
  uint32_t *t = calloc(1u << (d + KB), sizeof(uint32_t));
  uint32_t cs[K];
  uint32_t h = 0u;
  for (uint32_t n = 0u; n < R; ++n) {
    for (uint32_t j = 0u; j < (1u << D); ++j) {
      ks[j] = key(7u + n, j);
    }
    count(d, D - d, ks, t, cs);
    for (uint32_t c = 0u; c < K; ++c) {
      h = (h * 2654435761u) ^ cs[c];
    }
  }
  printf("%u\n", h);
  free(ks);
  free(t);
  return 0;
}
