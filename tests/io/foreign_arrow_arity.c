// bend-ids 1
// CID :twice BEND_CID_twice
// bend-ids end
// IO
// ==

Term twice_run(Env e, Term* f, IoWork* w) {
  return (Term)(uint32_t)(2 * (uint32_t)f[0]);
}

static void __attribute__((constructor)) twice_use(void) {
  io_eff(BEND_CID_twice, twice_run);
}
