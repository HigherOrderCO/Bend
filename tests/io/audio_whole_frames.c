// bend-ids 1
// CID :pause BEND_CID_pause
// bend-ids end
Term pause_run(Env e, Term* f, IoWork* w) {
  return f[0];
}

static void __attribute__((constructor)) pause_use(void) {
  io_eff(BEND_CID_pause, pause_run);
}
