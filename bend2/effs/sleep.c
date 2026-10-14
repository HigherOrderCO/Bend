// IO
// ==

static Term io_sleep_more(Env e, IoWork* w) {
  return term_pak(CID(Unit), 0);
}

// Parks until f[0] milliseconds from now.
Term io_sleep_run(Env e, Term* f, IoWork* w) {
  return io_wait_on(w, 0, 0, io_tick() + (u64)(u32)f[0] * 1000000ull,
    io_sleep_more);
}

static void __attribute__((constructor)) io_sleep_use(void) {
  io_eff(CID(IO.sleep), io_sleep_run);
}
