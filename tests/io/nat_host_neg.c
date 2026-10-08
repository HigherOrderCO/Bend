// bend-ids 1
// CID :get BEND_CID_get
// bend-ids end
Term get_run(Env e, Term* f, IoWork* w) {
  return (Term)(u64)-3;
}

static void __attribute__((constructor)) get_use(void) {
  io_eff(BEND_CID_get, get_run);
}
