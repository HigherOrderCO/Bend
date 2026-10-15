// A balanced tree of the depth asked, its leaves 1.. left to right, every
// odd one Empty; io_node seals the Node's fields as the program's own
// Node does, since the program shares the tree.
static Term shared_make_at(Env e, u32 depth, u32* next) {
  if (depth == 0) {
    *next += 1;
    return *next % 2 == 1 ? term_pak(CID(Empty), 0) : term_pak(CID(Leaf), *next);
  }
  Term l = shared_make_at(e, depth - 1, next);
  Term r = shared_make_at(e, depth - 1, next);
  return io_node(e, CID(Node), l, r);
}

Term shared_make_run(Env e, Term* f, IoWork* w) {
  u32 next = 0;
  return shared_make_at(e, (u32)f[0], &next);
}

static void __attribute__((constructor)) foreign_shared_use(void) {
  io_eff(CID(shared.make), shared_make_run);
}
