// The affine Tree and its singleton arrays are owned by this effect.
// Inspect every node, then perform the same leaf update as the JS twin.
Term exchange_run(Env e, Term* f, IoWork* w) {
  Term root = f[0];
  Term* at = &root;
  u32 depth = 0;
  while (term_aux(*at) == CID(Node)) {
    u64 loc = term_peek(e.mem, *at);
    if (e.mem[loc] != 123456789) abort();
    Term kids = e.mem[loc + 1];
    if (blk_span(kids) != 0) abort();
    at = &e.mem[blk_loc(e.mem, kids)];
    depth += 1;
  }
  if (depth != 20000 || term_aux(*at) != CID(Leaf)) abort();
  u64 leaf = term_peek(e.mem, *at);
  if (e.mem[leaf] != 123456789) abort();
  e.mem[leaf] = 987654321;
  return root;
}

static void __attribute__((constructor)) exchange_use(void) {
  io_eff(CID(exchange), exchange_run);
}
