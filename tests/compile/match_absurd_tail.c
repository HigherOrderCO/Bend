// bend-ids 1
// CID :keep BEND_CID_keep
// bend-ids end
static Term keep_run(Env e, Term* f, IoWork* w) {
  if (term_tag(f[0]) != TAG_CLO) {
    err_fail("expected a retained closure");
  }
  term_drop(e, f[0]);
  return (Term)1;
}

static void __attribute__((constructor)) keep_use(void) {
  io_eff(BEND_CID_keep, keep_run);
}
