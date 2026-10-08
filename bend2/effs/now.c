// bend-ids 1
// CID :IO.now BEND_CID_IO_now
// bend-ids end
// IO
// ==

Term io_now_run(Env e, Term* f, IoWork* w) {
  return (Term)(io_tick() / 1000000);
}

static void __attribute__((constructor)) io_now_use(void) {
  io_eff(BEND_CID_IO_now, io_now_run);
}
