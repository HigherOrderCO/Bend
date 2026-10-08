// bend-ids 1
// CID #Near BEND_CID_Near
// CID #Deep BEND_CID_Deep
// CID :far.make BEND_CID_far_make
// bend-ids end
// Far
// ===

// The constructors are named by this module's file, far_types, from
// wherever the program importing it sits.
static Term far_make_at(Env e, u32 depth, u32* next) {
  if (depth == 0) {
    *next += 1;
    return term_pak(BEND_CID_Near, *next);
  }
  Term l = far_make_at(e, depth - 1, next);
  Term r = far_make_at(e, depth - 1, next);
  return io_node(e, BEND_CID_Deep, l, r);
}

Term far_make_run(Env e, Term* f, IoWork* w) {
  u32 next = 0;
  return far_make_at(e, (u32)f[0], &next);
}

static void __attribute__((constructor)) far_make_use(void) {
  io_eff(BEND_CID_far_make, far_make_run);
}
