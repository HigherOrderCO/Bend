static Term readiness_closed_run(Env e, Term* f, IoWork* w) {
  int fd = socket(AF_INET, SOCK_DGRAM, 0);
  if (fd < 0) return io_fail(e, (u32)errno, NULL);
  if (close(fd) < 0) return io_fail(e, (u32)errno, NULL);
  return io_done(e, io_hand(fd));
}

static Term readiness_idle_more(Env e, IoWork* w) {
  return term_pak(CID(Unit), 0);
}

static Term readiness_idle_run(Env e, Term* f, IoWork* w) {
  int p[2];
  if (pipe(p) < 0) err_fail("the test pipe failed");
  return io_wait_on(w, p[0], POLLIN, 0, readiness_idle_more);
}

static void __attribute__((constructor)) readiness_use(void) {
  io_eff(CID(Readiness.closed), readiness_closed_run);
  io_eff(CID(Readiness.idle), readiness_idle_run);
}
