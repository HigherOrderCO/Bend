Term pause_run(Env e, Term* f, IoWork* w) {
  return f[0];
}

static void __attribute__((constructor)) pause_use(void) {
  io_eff(CID(pause), pause_run);
}
