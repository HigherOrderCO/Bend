// bend-ids 1
// CID :flags BEND_CID_flags
// CID #Unit BEND_CID_Unit
// bend-ids end
static Term nofk_flags_run(Env e, Term* f, IoWork* w) {
  if (term_tag(f[0]) != TAG_CLO || term_tag(f[1]) != TAG_CLO) {
    err_fail("expected retained functions");
  }
  if (fid_nofk(term_aux(f[0])) || !fid_nofk(term_aux(f[1]))) {
    err_fail("incorrect transitive NOFK metadata");
  }
  term_drop(e, f[0]);
  term_drop(e, f[1]);
  return term_pak(BEND_CID_Unit, 0);
}

static void __attribute__((constructor)) nofk_flags_use(void) {
  io_eff(BEND_CID_flags, nofk_flags_run);
}
