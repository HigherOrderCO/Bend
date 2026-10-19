static Term $CID(Term n) { return n + 1; }
static Term πCID(Term n) { return n + 1; }

#define ID_OF(x) x##CID(answer)

static Term answer_run(Env e, Term* f, IoWork* w) {
  (void)e;
  (void)f;
  (void)w;
  return $CID(20) + πCID(20);
}

static void __attribute__((constructor)) answer_use(void) {
  io_eff(ID_OF(), answer_run);
}

#ifdef CID(helper)
static Term helper_run(Env e, Term* f, IoWork* w) {
  (void)e;
  (void)f;
  (void)w;
  return 42;
}
static void __attribute__((constructor)) helper_use(void) {
  io_eff(CID(helper), helper_run);
}
#endif
