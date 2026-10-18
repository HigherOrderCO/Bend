static Term $CID(Term n) { return n + 1; }
static Term πCID(Term n) { return n + 1; }

static Term answer_run(Env e, Term* f, IoWork* w) {
  (void)e;
  (void)f;
  (void)w;
  return $CID(20) + πCID(20);
}

static void __attribute__((constructor)) answer_use(void) {
  io_eff(CID(answer), answer_run);
}
